package com.abraxas.slothql.cypher

import scala.language.higherKinds

import cats.effect.IO
import cats.free.Free
import cats.free.Free.liftF
import shapeless.DepFn1

import com.abraxas.slothql.cypher.CypherFragment.{ Known, Query }

trait CypherQueryExecutor {
  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]

  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]]
}

trait CypherTransactor extends CypherQueryExecutor {

  def read[A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[read.Out] =
    liftF[Read, read.Out](Read[A](query))
  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]] =
    runRead(read(query))

  // Run transactions
  def run[A](tx: Tx[A]): IO[Seq[A]]
  def runRead[A](tx: ReadTx[A]): IO[Seq[A]]
  def runWrite[A](tx: WriteTx[A]): IO[Seq[A]]


  sealed trait Operation[R]
  sealed trait Read[R] extends Operation[Seq[R]] {
    type A
    val query: Known[Query[A]]
    val reader: CypherTransactor.Reader.Aux[Result, A, R]
  }
  sealed trait ReadWrite[R] extends Read[R] {
    override val query: Known[Query[A]] // TODO: `Query[A]`
  }

  protected object Read {
    type Aux[R, A0] = Read[R] { type A = A0 }
    def apply[A0](q: Known[Query[A0]])(implicit r: Reader[A0]): Aux[r.Out, A0] =
      new Read[r.Out] {
        type A = A0
        val query: Known[Query[A]] = q
        val reader: CypherTransactor.Reader.Aux[Result, A, r.Out] = r
      }
  }

  type Tx[R] = Free[Operation, R]
  type ReadTx[R] = Free[Read, R]
  type WriteTx[R] = Free[ReadWrite, R]
}

object CypherTransactor {
  type Aux[R, RR[x] <: Reader[R, x]] = CypherTransactor { type Result = R; type Reader[A] = RR[A] }

  trait Reader[Src, A] extends DepFn1[Src]
  object Reader {
    type Aux[Src, A, R] = Reader[Src, A] { type Out = R }
    def apply[Src, A](implicit reader: Reader[Src, A]): Aux[Src, A, reader.Out] = reader
  }
}
