package com.abraxas.slothql.cypher

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import cats.effect.IO
import cats.free.FreeT
import cats.{ Alternative, Functor, FunctorFilter, Id, Monad, MonoidK, StackSafeMonad, Traverse, ~> }
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

  type Read[F[_], R] = txBuilder.Read[F, R]
  type ReadWrite[F[_], R] = txBuilder.ReadWrite[F, R]

  final lazy val Read = txBuilder.Read

  final def read[F[_]]: ReadBuilder[F] = ReadBuilder.asInstanceOf[ReadBuilder[F]]

  protected class ReadBuilder[F[_]] {
    def apply[A](query: Known[Query[A]])(implicit M: Monad[F], T: Traverse[F], readTo: ReadTo[F], read: Reader[A]): ReadTx[F, read.Out] =
      txBuilder.read(query)

    def apply[Params <: HList, A](query: Parameterized.Prepared[Params, Query[A]])
                                 (implicit M: Monad[F], T: Traverse[F], readTo: ReadTo[F], read: Reader[A], supported: SupportedParams[Params])
                                 : ParameterizedReadQueryBuilder[Params, F, A, read.Out] =
      txBuilder.read(query)
  }
  private object ReadBuilder extends ReadBuilder[Id]

  def readIO[F[_]: Monad: Traverse: ReadTo, A](query: Known[Query[A]])(implicit r: Reader[A]): IO[F[r.Out]] =
    runRead(read[F](query))

  // Run transactions
  def run[F[_]: Monad: Traverse, A, Tx: (ReadTx[F, A] |∨| WriteTx[F, A])#λ](tx: Tx)(
    implicit isRead: Tx <:< ReadTx[F, _] = null, isWrite: Tx <:< WriteTx[F, _] = null
  ): IO[F[A]] =
    tx match {
      case tx: ReadTx [F, A] @unchecked if isRead  != null => runRead(tx)
      case tx: WriteTx[F, A] @unchecked if isWrite != null => runWrite(tx)
    }
  def runRead [F[_]: Monad: Traverse, A](tx: ReadTx [F, A]): IO[F[A]]
  def runWrite[F[_]: Monad: Traverse, A](tx: WriteTx[F, A]): IO[F[A]]

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
  sealed trait Read     [F[_], R] extends Operation[F[R]]
  sealed trait ReadWrite[F[_], R] extends Read[F, R]

  type ReadTo[F[_]] = CanBuildFrom[Nothing, Any, F[_]]

  case class ReadQuery[F[_], A, R](
      query: Known[Query[A]],
      readTo: ReadTo[F],
      reader: CypherTransactor.Reader.Aux[Result, A, R]
  ) extends Read[F, R]
  case class PreparedReadQuery[F[_], A, R](
      statement: CypherFragment.Statement,
      readTo: ReadTo[F],
      reader: CypherTransactor.Reader.Aux[Result, A, R]
  ) extends Read[F, R]

  case class Gather[F[_], R](read: Read[F, R]) extends Read[F, F[R]]
  case class Unwind[F[_], R](values: F[R]) extends Read[F, R]

  object Read {
    def apply[F[_], A](q: Known[Query[A]])(implicit b: ReadTo[F], r: Reader[A]): ReadQuery[F, A, r.Out] = ReadQuery(q, b, r)

    def const [F[_]]: Id ~> ReadTx[F, ?] = λ[Id ~> ReadTx[F, ?]](FreeT.pure(_))
    def liftIO[F[_]]: IO ~> ReadTx[F, ?] = λ[IO ~> ReadTx[F, ?]](FreeT.liftT(_))

    def product[F[_], A, B](txA: ReadTx[F, A], txB: ReadTx[F, B]): ReadTx[F, (A, B)] = (txA, txB).tupled

    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[F[x] <: Seq[x]: Monad: Traverse, A, B](txA: ReadTx[F, A], txB: ReadTx[F, B]): ReadTx[F, (A, B)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        _ = if (as.length != bs.length) throw new CannotZipException(as, bs)
        zipped <- unwind(MoreZip.zip2(as, bs))
      } yield zipped

    /** Will throw [[CannotZip3Exception]] if lengths of the results do not correspond */
    def zip3[F[x] <: Seq[x]: Monad: Traverse, A, B, C](txA: ReadTx[F, A], txB: ReadTx[F, B], txC: ReadTx[F, C]): ReadTx[F, (A, B, C)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        cs <- txC.gather
        _ = if (as.length != bs.length || bs.length != cs.length) throw new CannotZip3Exception(as, bs, cs)
        zipped <- unwind(MoreZip.zip3(as, bs, cs))
      } yield zipped

    def filterOpt[F[_]: MonoidK, A, B] (tx: ReadTx[F, A])(f: A => Option[B]) : ReadTx[F, B] = tx.flatMap { r => f(r).map(const[F](_)).getOrElse(nothing) }
    def filter   [F[_]: MonoidK, A]    (tx: ReadTx[F, A])(pred: A => Boolean): ReadTx[F, A] = tx.flatMap { r => if (pred(r)) const(r) else nothing }
    def filtering[F[_]: Alternative, A](tx: ReadTx[F, A])(predTx: A => ReadTx[F, Boolean]): ReadTx[F, A] =
      for {
        a <- tx
        b <- predTx(a)
        if b
      } yield a

    def unwind [F[_], A](iterable: F[A])        : ReadTx[F, A] = FreeT.liftF(Unwind(iterable))
    def nothing[F[_], A](implicit M: MonoidK[F]): ReadTx[F, A] = unwind(M.empty)

    class CannotZipException(left: Seq[Any], right: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\tleft:  $left\n\tright: $right")

    class CannotZip3Exception(seq1: Seq[Any], seq2: Seq[Any], seq3: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\t1: $seq1\n\t2: $seq2\n\t3: $seq3")


    implicit def readTxTraversableMonadIsMonad[F[_]: Monad: Traverse]: Monad[λ[A => ReadTx[F, F[A]]]] =
      new StackSafeMonad[λ[A => ReadTx[F, F[A]]]]{
        def pure[A](x: A): ReadTx[F, F[A]] =
          FreeT.pure(x.pure[F])
        override def map[A, B](fa: ReadTx[F, F[A]])(f: A => B): ReadTx[F, F[B]] =
          fa.map(Monad[F].map(_)(f))
        def flatMap[A, B](fa: ReadTx[F, F[A]])(f: A => ReadTx[F, F[B]]): ReadTx[F, F[B]] =
          fa.flatMap(_.flatTraverse(f))
      }

    implicit def readTxIsFunctorFilter[F[_]: MonoidK]: FunctorFilter[ReadTx[F, ?]] = new FunctorFilter[ReadTx[F, ?]]{
      val functor: Functor[ReadTx[F, ?]] = Functor[ReadTx[F, ?]]
      def mapFilter[A, B](fa: ReadTx[F, A])(f: A => Option[B]): ReadTx[F, B] = filterOpt(fa)(f)
    }


    implicit class ReadTxOps[F[_], A](tx: ReadTx[F, A]) {
      def filtering   (predTx: A => ReadTx[F, Boolean])(implicit M: Alternative[F]): ReadTx[F, A] = Read.filtering(tx)(predTx)
      def filter      (pred: A => Boolean)             (implicit M: MonoidK[F])    : ReadTx[F, A] = Read.filter(tx)(pred)
      def withFilter  (pred: A => Boolean)             (implicit M: MonoidK[F])    : ReadTx[F, A] = filter(pred)
      def filterOpt[B](f: A => Option[B])              (implicit M: MonoidK[F])    : ReadTx[F, B] = Read.filterOpt(tx)(f)

      def x[B](that: ReadTx[F, B]): ReadTx[F, (A, B)] = Read.product(tx, that)

      def gather(implicit M: Monad[F], T: Traverse[F]): ReadTx[F, F[A]] =
        tx.mapK[ReadTxF](λ[IO         ~> ReadTxF]{ io => FreeT.liftT(io.map(_.pure[F])) })
          .foldMap      (λ[Read[F, ?] ~> ReadTxF]{ fa => FreeT.liftF(newGather(fa))     })

      private type ReadTxF[X] = ReadTx[F, F[X]]
      private def newGather[X](read: Read[F, X]): Read[F, F[X]] = Gather(read)
    }
  }

  implicit class ReadTxZipOps[F[x] <: Seq[x]: Monad: Traverse, A](tx: ReadTx[F, A]) {
    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[B](that: ReadTx[F, B]): ReadTx[F, (A, B)] = Read.zip(tx, that)
  }


  type ReadTx [F[_], R] = FreeT[Read     [F, ?], IO, R]
  type WriteTx[F[_], R] = FreeT[ReadWrite[F, ?], IO, R]


  def read[F[_]: ReadTo, A](query: Known[Query[A]])(implicit read: Reader[A]): ReadTx[F, read.Out] =
    FreeT.liftF(Read[F, A](query))

  def read[F[_], Params <: HList, A](query: Parameterized.Prepared[Params, Query[A]])
                                    (implicit readTo: ReadTo[F], reader: Reader[A], supported: SupportedParams[Params])
                                    : ParameterizedReadQueryBuilder[Params, F, A, reader.Out] =
    new ParameterizedReadQueryBuilder(query)(reader, readTo, supported)

  final class ParameterizedReadQueryBuilder[Params <: HList, F[_], A, R]
      (q: Parameterized.Prepared[Params, Query[A]])
      (implicit val reader: ReaderAux[A, R], readTo: ReadTo[F], supported: SupportedParams[Params])
    extends RecordArgs
  {
    def withParamsRecord(params: Params): Read[F, R] = {
      import supported._
      val statement = q.changeParams[Poly](mapper)
                       .applyRecord(mapper(params))(toMap)
      PreparedReadQuery(statement, readTo, reader)
    }

    def withParamsTxRecord(params: Params): ReadTx[F, R] = FreeT.liftF(withParamsRecord(params))
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