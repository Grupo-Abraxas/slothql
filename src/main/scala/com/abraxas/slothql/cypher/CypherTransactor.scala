package com.abraxas.slothql.cypher

import scala.language.higherKinds

import cats.effect.IO
import cats.free.Free
import cats.free.Free.liftF
import cats.{ Monad, ~> }
import cats.instances.vector._
import cats.syntax.traverse._
import shapeless.DepFn1

import com.abraxas.slothql.cypher.CypherFragment.{ Known, Query }

trait CypherQueryExecutor {
  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]

  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]]
}

trait CypherTransactor extends CypherQueryExecutor with CypherTxBuilder {
  type Transactor = this.type

  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]] =
    runRead(read(query))

  // Run transactions
  def run[A](tx: Tx[A]): IO[Seq[A]]
  def runRead[A](tx: ReadTx[A]): IO[Seq[A]]
  def runWrite[A](tx: WriteTx[A]): IO[Seq[A]]

}

object CypherTransactor {
  type Aux[R, RR[x] <: Reader[R, x]] = CypherTransactor { type Result = R; type Reader[A] = RR[A] }

  trait Reader[Src, A] extends DepFn1[Src]
  object Reader {
    type Aux[Src, A, R] = Reader[Src, A] { type Out = R }
    def apply[Src, A](implicit reader: Reader[Src, A]): Aux[Src, A, reader.Out] = reader
  }
}


trait CypherTxBuilder {
  type Transactor <: CypherTransactor

  sealed trait Operation[R]
  sealed trait Read[R] extends Operation[Seq[R]] {
    type A
    val query: Known[Query[A]]
    val reader: CypherTransactor.Reader.Aux[Transactor#Result, A, R]
  }
  sealed trait ReadWrite[R] extends Read[R] {
    override val query: Known[Query[A]] // TODO: `Query[A]`
  }

  protected case class Gather[R](read: Read[R]) extends Read[Vector[R]] {
    type A = read.A
    lazy val query: Known[Query[read.A]] = ???
    lazy val reader: CypherTransactor.Reader.Aux[Transactor#Result, read.A, Vector[R]] = ???
  }

  protected object Read {
    type Aux[R, A0] = Read[R] { type A = A0 }
    def apply[A0](q: Known[Query[A0]])(implicit r: Transactor#Reader[A0]): Aux[r.Out, A0] =
      new Read[r.Out] {
        type A = A0
        val query: Known[Query[A]] = q
        val reader: CypherTransactor.Reader.Aux[Transactor#Result, A, r.Out] =
          r.asInstanceOf[CypherTransactor.Reader.Aux[Transactor#Result, A, r.Out]]
      }


    implicit object ReadTxVectorIsMonad extends Monad[λ[A => ReadTx[Vector[A]]]] {
      def pure[A](x: A): ReadTx[Vector[A]] =
        Free.pure(Vector(x))
      override def map[A, B](fa: ReadTx[Vector[A]])(f: A => B): ReadTx[Vector[B]] =
        fa.map(_.map(f))
      def flatMap[A, B](fa: ReadTx[Vector[A]])(f: A => ReadTx[Vector[B]]): ReadTx[Vector[B]] =
        fa.flatMap(_.flatTraverse(f))

      // TODO: Is it tail-call optimised? (I doubt it)
      def tailRecM[A, B](a0: A)(f: A => ReadTx[Vector[Either[A, B]]]): ReadTx[Vector[B]] =
        f(a0) flatMap {
          _.flatTraverse[ReadTx, B] {
            case Left(a)  => tailRecM(a)(f)
            case Right(b) => Free.pure(Vector(b))
          }
        }
    }


    implicit class ReadTxOps[R](tx: ReadTx[R]) {
      def gather: ReadTx[Vector[R]] = tx.foldMap[λ[A => ReadTx[Vector[A]]]](
        λ[Read ~> λ[A => ReadTx[Vector[A]]]](fa => liftF(newGather(fa)))
      )
    }

    private def newGather[R](read: Read[R]): Read[Vector[R]] = Gather(read)
  }


  type Tx[R] = Free[Operation, R]
  type ReadTx[R] = Free[Read, R]
  type WriteTx[R] = Free[ReadWrite, R]


  def read[A](query: Known[Query[A]])(implicit read: Transactor#Reader[A]): ReadTx[read.Out] =
    liftF[Read, read.Out](Read[A](query))
}