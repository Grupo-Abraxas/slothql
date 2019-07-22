package com.abraxas.slothql.cypher

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import cats.effect.IO
import cats.free.Free
import cats.free.Free.liftF
import cats.{ Functor, FunctorFilter, Monad, Traverse, ~> }
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import shapeless.ops.record.{ MapValues, ToMap }
import shapeless.{ DepFn1, HList, Poly1, RecordArgs, ops, |∨| }

import com.abraxas.slothql.cypher.CypherFragment.{ Known, Parameterized, Query }
import com.abraxas.slothql.util.MoreZip

trait CypherTransactor {
  type TxBuilder <: CypherTxBuilder
  val txBuilder: TxBuilder
  import txBuilder._

  type Operation[R] = txBuilder.Operation[R]
  type Read[R]      = txBuilder.Read[R]
  type ReadWrite[R] = txBuilder.ReadWrite[R]

  final lazy val Read   = txBuilder.Read

  final def read[A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[read.Out] = txBuilder.read(query)
  final def read[Params <: HList, A](query: Parameterized.Prepared[Params, Query[A]])
                                    (implicit read: Reader[A], supported: SupportedParams[Params]): ParameterizedReadQueryBuilder[Params, A, read.Out] =
    txBuilder.read(query)

  def readIO[A](query: Known[Query[A]])(implicit r: Reader[A]): IO[Seq[r.Out]] =
    runRead(read(query))

  // Run transactions
  def run[A, Tx: (ReadTx[A] |∨| WriteTx[A])#λ](tx: Tx)(
    implicit isRead: Tx <:< ReadTx[_] = null, isWrite: Tx <:< WriteTx[_] = null
  ): IO[Seq[A]] =
    tx match {
      case tx: ReadTx[A]  @unchecked if isRead  != null => runRead(tx)
      case tx: WriteTx[A] @unchecked if isWrite != null => runWrite(tx)
    }
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
  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]
  type ReaderAux[A, R] = Reader[A] { type Out = R }

  type CypherQuery[+A] = CypherFragment.Query[A]

  sealed trait Operation[R]
  sealed trait Read[R] extends Operation[Seq[R]]
  sealed trait ReadWrite[R] extends Read[R]

  case class ReadQuery[A, R](query: Known[Query[A]], reader: CypherTransactor.Reader.Aux[Result, A, R]) extends Read[R]
  case class PreparedReadQuery[A, R](statement: CypherFragment.Statement, reader: CypherTransactor.Reader.Aux[Result, A, R]) extends Read[R]

  case class Gather[F[_] <: Iterable[_], R](read: Read[R])
                                           (val fromIterable: Iterable[R] => F[R]) extends Read[F[R]] { type CC[_] = F[_] }
  object Gather {
    def apply[F[_] <: Iterable[_], R](read: Read[R])
                                     (implicit cbf: CanBuildFrom[Nothing, Any, F[_]]): Gather[F, R] =
      new Gather(read)(xs => (cbf.apply() ++= xs).result().asInstanceOf[F[R]])
  }
  case class Unwind[R](values: Iterable[R]) extends Read[R]

  case class LiftIoTx[R](io: IO[ReadTx[R]]) extends Read[R]

  object Read {
    def apply[A](q: Known[Query[A]])(implicit r: Reader[A]): ReadQuery[A, r.Out] = ReadQuery(q, r)

    def const[A](a: A): ReadTx[A] = Free.pure(a)

    def liftIo  [R](io: IO[Read[R]]  ): ReadTx[R] = liftF(LiftIoTx(io.map(liftF)))
    def liftIoTx[R](io: IO[ReadTx[R]]): ReadTx[R] = liftF(LiftIoTx(io))

    def product[A, B](txA: ReadTx[A], txB: ReadTx[B]): ReadTx[(A, B)] = (txA, txB).tupled

    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[A, B](txA: ReadTx[A], txB: ReadTx[B]): ReadTx[(A, B)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        _ = if (as.length != bs.length) throw new CannotZipException(as, bs)
        zipped <- unwind(as zip bs)
      } yield zipped

    /** Will throw [[CannotZip3Exception]] if lengths of the results do not correspond */
    def zip3[A, B, C](txA: ReadTx[A], txB: ReadTx[B], txC: ReadTx[C]): ReadTx[(A, B, C)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        cs <- txC.gather
        _ = if (as.length != bs.length || bs.length != cs.length) throw new CannotZip3Exception(as, bs, cs)
        zipped <- unwind(MoreZip.zip3(as, bs, cs))
      } yield zipped

    def filterOpt[A, B](tx: ReadTx[A])(f: A => Option[B]): ReadTx[B]  = tx.flatMap { r => f(r).map(const).getOrElse(nothing) }
    def filter   [A]   (tx: ReadTx[A])(pred: A => Boolean): ReadTx[A] = tx.flatMap { r => if (pred(r)) const(r) else nothing }
    def filtering[A]   (tx: ReadTx[A])(predTx: A => ReadTx[Boolean]): ReadTx[A] =
      for {
        a <- tx
        b <- predTx(a)
        if b
      } yield a

    def unwind[A](iterable: Iterable[A]): ReadTx[A] = liftF(Unwind(iterable))
    def nothing[A]: ReadTx[A] = unwind(Nil)

    class CannotZipException(left: Seq[Any], right: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\tleft:  $left\n\tright: $right")

    class CannotZip3Exception(seq1: Seq[Any], seq2: Seq[Any], seq3: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\t1: $seq1\n\t2: $seq2\n\t3: $seq3")


    implicit def readTxTraversableMonadIsMonad[F[_]: Monad: Traverse]: Monad[λ[A => ReadTx[F[A]]]] = new Monad[λ[A => ReadTx[F[A]]]]{
      def pure[A](x: A): ReadTx[F[A]] =
        Free.pure(x.pure[F])
      override def map[A, B](fa: ReadTx[F[A]])(f: A => B): ReadTx[F[B]] =
        fa.map(Monad[F].map(_)(f))
      def flatMap[A, B](fa: ReadTx[F[A]])(f: A => ReadTx[F[B]]): ReadTx[F[B]] =
        fa.flatMap(_.flatTraverse(f))

      // TODO: Is it tail-call optimised? (I doubt it)
      def tailRecM[A, B](a0: A)(f: A => ReadTx[F[Either[A, B]]]): ReadTx[F[B]] =
        f(a0) flatMap {
          _.flatTraverse[ReadTx, B] {
            case Left(a)  => tailRecM(a)(f)
            case Right(b) => Free.pure(b.pure[F])
          }
        }
    }

    implicit object ReadTxIsFunctorFilter extends FunctorFilter[ReadTx] {
      val functor: Functor[ReadTx] = Functor[ReadTx]
      def mapFilter[A, B](fa: ReadTx[A])(f: A => Option[B]): ReadTx[B] = filterOpt(fa)(f)
    }


    implicit class ReadTxOps[A](tx: ReadTx[A]) {
      def gather[F[_] <: Iterable[_]: Monad: Traverse]
                (implicit cbf: CanBuildFrom[Nothing, Any, F[_]]): ReadTx[F[A]] =
        tx.foldMap[λ[X => ReadTx[F[X]]]](
          λ[Read ~> λ[X => ReadTx[F[X]]]](fa => liftF(newGather(fa)))
        )
      /** Gather rows to [[Vector]] by default. */
      def gather: ReadTx[Vector[A]] = gather[Vector]

      def filtering(predTx: A => ReadTx[Boolean]): ReadTx[A] = Read.filtering(tx)(predTx)
      def filter(pred: A => Boolean): ReadTx[A] = Read.filter(tx)(pred)
      def withFilter(pred: A => Boolean): ReadTx[A] = filter(pred)
      def filterOpt[B](f: A => Option[B]): ReadTx[B] = Read.filterOpt(tx)(f)

      def x[B](that: ReadTx[B]): ReadTx[(A, B)] = Read.product(tx, that)

      /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
      def zip[B](that: ReadTx[B]): ReadTx[(A, B)] = Read.zip(tx, that)
    }

    private def newGather[F[_] <: Iterable[_], R](read: Read[R])(implicit cbf: CanBuildFrom[Nothing, Any, F[_]]): Read[F[R]] = Gather[F, R](read)
  }


  type ReadTx[R] = Free[Read, R]
  type WriteTx[R] = Free[ReadWrite, R]


  def read[A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[read.Out] =
    liftF[Read, read.Out](Read[A](query))

  def read[Params <: HList, A](query: Parameterized.Prepared[Params, Query[A]])
                              (implicit reader: Reader[A], supported: SupportedParams[Params]): ParameterizedReadQueryBuilder[Params, A, reader.Out] =
    new ParameterizedReadQueryBuilder(query)(reader, supported)

  final class ParameterizedReadQueryBuilder[Params <: HList, A, R](q: Parameterized.Prepared[Params, Query[A]])
                                                                  (implicit val reader: ReaderAux[A, R], supported: SupportedParams[Params]) extends RecordArgs {
    def withParamsRecord(params: Params): Read[R] = {
      import supported._
      val statement = q.changeParams[Poly](mapper)
                       .applyRecord(mapper(params))(toMap)
      PreparedReadQuery(statement, reader)
    }

    def withParamsTxRecord(params: Params): ReadTx[R] = liftF[Read, R](withParamsRecord(params))
  }

  sealed trait SupportedParams[Params0 <: HList] {
    type Params <: HList

    type Poly = SupportedParams.Poly.type

    def mapper: ops.record.MapValues.Aux[Poly, Params0, Params]
    def toMap: ops.record.ToMap.Aux[Params, _ <: Symbol, _ <: Any]
  }
  object SupportedParams {
    type Aux[Params0 <: HList, Params1 <: HList] = SupportedParams[Params0] { type Params = Params1 }

    object Poly extends Poly1 {
      implicit def impl[A](implicit supported: SupportedParam[A]): Case.Aux[A, supported.Out] = at[A](supported(_))
    }

    implicit def supportedParams[Params0 <: HList, Params1 <: HList](
      implicit
      mapper0: ops.record.MapValues.Aux[Poly.type, Params0, Params1],
      toMap0: ops.record.ToMap.Aux[Params1, _ <: Symbol, _ <: Any]
    ): SupportedParams.Aux[Params0, Params1] =
      new SupportedParams[Params0] {
        type Params = Params1
        def mapper: MapValues.Aux[Poly.type, Params0, Params1] = mapper0
        def toMap: ToMap.Aux[Params1, _ <: Symbol, _] = toMap0
      }
  }

  trait SupportedParam[A] extends DepFn1[A]
  object SupportedParam {
    type Aux[A, R] = SupportedParam[A] { type Out = R }

    def define[A, R](f: A => R): SupportedParam.Aux[A, R] =
      new SupportedParam[A] {
        type Out = R
        def apply(t: A): R = f(t)
      }

    implicit def intParamIsSupported: SupportedParam.Aux[Int, Int] = define(locally)
    implicit def longParamIsSupported: SupportedParam.Aux[Long, Long] = define(locally)
    implicit def stringParamIsSupported: SupportedParam.Aux[String, String] = define(locally)
    implicit def booleanParamIsSupported: SupportedParam.Aux[Boolean, Boolean] = define(locally)

    implicit def seqParamIsSupported[A0, A](implicit isSeq: A0 <:< Seq[A], underlying: SupportedParam[A]): SupportedParam.Aux[A0, java.util.List[underlying.Out]] =
      define(_.map(underlying(_)).asJava)

    implicit def mapParamIsSupported[A](implicit underlying: SupportedParam[A]): SupportedParam.Aux[Map[String, A], java.util.Map[String, underlying.Out]] =
      define(_.mapValues(underlying(_)).asJava)
  }

}