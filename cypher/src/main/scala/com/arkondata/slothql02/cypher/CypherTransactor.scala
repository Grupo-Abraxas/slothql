/*
package com.arkondata.slothql02.cypher

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
import shapeless.{ DepFn1, HList, Poly1, RecordArgs, ops }

import com.arkondata.slothql02.cypher.{ CypherFragment => CF }
import com.arkondata.slothql02.cypher.CypherFragment.{ Known, Parameterized }
import com.arkondata.slothql02.util.MoreZip

trait CypherTransactor {
  type TxBuilder <: CypherTxBuilder
  val txBuilder: TxBuilder
  import txBuilder._

  type Op[F[_], R] = txBuilder.Operation[F, R]
  final lazy val Op = txBuilder.Op

  final def query[F[_]]: QueryBuilder[F] = txBuilder.query[F]

  def readIO[F[_]: Monad: Traverse: ReadTo, A](query: Known[CF.Query[A]])(implicit r: Reader[A]): IO[F[r.Out]] =
    runRead(txBuilder.query[F](query))
  def writeIO[F[_]: Monad: Traverse: ReadTo, A](query: Known[CF.Query[A]])(implicit r: Reader[A]): IO[F[r.Out]] =
    runWrite(txBuilder.query[F](query))

  // Run transactions
  def runRead [F[_]: Monad: Traverse, A](tx: Tx[F, A]): IO[F[A]]
  def runWrite[F[_]: Monad: Traverse, A](tx: Tx[F, A]): IO[F[A]]

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
  type Tx[F[_], R] = FreeT[Operation[F, ?],  IO, R]

  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]
  type ReaderAux[A, R] = Reader[A] { type Out = R }

  type CypherQuery[+A] = CF.Query[A]

  type ReadTo[F[_]] = CanBuildFrom[Nothing, Any, F[_]]

  sealed trait Operation[F[_], R]

  case class Query[F[_], A, R](
      query: Known[CF.Query[A]],
      readTo: ReadTo[F],
      reader: CypherTransactor.Reader.Aux[Result, A, R]
  ) extends Operation[F, R]

  case class PreparedQuery[F[_], A, R](
      statement: CF.Statement,
      readTo: ReadTo[F],
      reader: CypherTransactor.Reader.Aux[Result, A, R]
  ) extends Operation[F, R]

  case class Gather[F[_], R](op: Operation[F, R]) extends Operation[F, F[R]]
  case class Unwind[F[_], R](values: F[R]) extends Operation[F, R]

  object Operation {
    implicit def txTraversableMonadIsMonad[F[_]: Monad: Traverse]: Monad[λ[A => Tx[F, F[A]]]] =
      new StackSafeMonad[λ[A => Tx[F, F[A]]]]{
        def pure[A](x: A): Tx[F, F[A]] =
          FreeT.pure(x.pure[F])
        override def map[A, B](fa: Tx[F, F[A]])(f: A => B): Tx[F, F[B]] =
          fa.map(Monad[F].map(_)(f))
        def flatMap[A, B](fa: Tx[F, F[A]])(f: A => Tx[F, F[B]]): Tx[F, F[B]] =
          fa.flatMap(_.flatTraverse(f))
      }

    implicit def txIsFunctorFilter[F[_]: MonoidK]: FunctorFilter[Tx[F, ?]] = new FunctorFilter[Tx[F, ?]]{
      val functor: Functor[Tx[F, ?]] = Functor[Tx[F, ?]]
      def mapFilter[A, B](fa: Tx[F, A])(f: A => Option[B]): Tx[F, B] = Op.filterOpt(fa)(f)
    }


    implicit class TxOps[F[_], A](tx: Tx[F, A]) {
      def filtering(predTx: A => Tx[F, Boolean])(implicit M: Alternative[F]): Tx[F, A] = Op.filtering(tx)(predTx)
      def filter      (pred: A => Boolean)      (implicit M: MonoidK[F])    : Tx[F, A] = Op.filter(tx)(pred)
      def withFilter  (pred: A => Boolean)      (implicit M: MonoidK[F])    : Tx[F, A] = filter(pred)
      def filterOpt[B](f: A => Option[B])       (implicit M: MonoidK[F])    : Tx[F, B] = Op.filterOpt(tx)(f)

      def x[B](that: Tx[F, B]): Tx[F, (A, B)] = Op.product(tx, that)

      def gather(implicit M: Monad[F], T: Traverse[F]): Tx[F, F[A]] =
        tx.mapK[TxF](λ[IO         ~> TxF]{ io => FreeT.liftT(io.map(_.pure[F])) })
          .foldMap  (λ[Operation[F, ?] ~> TxF]{ fa => FreeT.liftF(newGather(fa))     })

      private type TxF[X] = Tx[F, F[X]]
      private def newGather[X](op: Operation[F, X]): Operation[F, F[X]] = Gather(op)
    }
  }

  object Op {
    def query[F[_], A](q: Known[CF.Query[A]])(implicit b: ReadTo[F], r: Reader[A]): Tx[F, r.Out] = FreeT.liftF(Query(q, b, r))
    def prepared[F[_], A](s: CF.Statement)(implicit b: ReadTo[F], r: Reader[A]): Tx[F, r.Out] = FreeT.liftF(PreparedQuery(s, b, r))

    def const [F[_]]: Id ~> Tx[F, ?] = λ[Id ~> Tx[F, ?]](FreeT.pure(_))
    def liftIO[F[_]]: IO ~> Tx[F, ?] = λ[IO ~> Tx[F, ?]](FreeT.liftT(_))
    def liftTx[F[_]]: λ[A => IO[Tx[F, A]]] ~> Tx[F, ?] =
      λ[λ[A => IO[Tx[F, A]]] ~> Tx[F, ?]]{ io => FreeT.liftT(io).flatMap(locally) }

    def product[F[_], A, B](txA: Tx[F, A], txB: Tx[F, B]): Tx[F, (A, B)] = (txA, txB).tupled

    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[F[x] <: Seq[x]: Monad: Traverse, A, B](txA: Tx[F, A], txB: Tx[F, B]): Tx[F, (A, B)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        _ = if (as.length != bs.length) throw new CannotZipException(as, bs)
        zipped <- unwind(MoreZip.zip2(as, bs))
      } yield zipped

    /** Will throw [[CannotZip3Exception]] if lengths of the results do not correspond */
    def zip3[F[x] <: Seq[x]: Monad: Traverse, A, B, C](txA: Tx[F, A], txB: Tx[F, B], txC: Tx[F, C]): Tx[F, (A, B, C)] =
      for {
        as <- txA.gather
        bs <- txB.gather
        cs <- txC.gather
        _ = if (as.length != bs.length || bs.length != cs.length) throw new CannotZip3Exception(as, bs, cs)
        zipped <- unwind(MoreZip.zip3(as, bs, cs))
      } yield zipped

    def filterOpt[F[_]: MonoidK, A, B](tx: Tx[F, A])(f: A => Option[B]) : Tx[F, B] = tx.flatMap { r => f(r).map(const[F](_)).getOrElse(nothing) }
    def filter   [F[_]: MonoidK, A](tx: Tx[F, A])(pred: A => Boolean): Tx[F, A] = tx.flatMap { r => if (pred(r)) const(r) else nothing }
    def filtering[F[_]: Alternative, A](tx: Tx[F, A])(predTx: A => Tx[F, Boolean]): Tx[F, A] =
      for {
        a <- tx
        b <- predTx(a)
        if b
      } yield a

    def unwind [F[_], A](iterable: F[A])        : Tx[F, A] = FreeT.liftF(Unwind(iterable))
    def nothing[F[_], A](implicit M: MonoidK[F]): Tx[F, A] = unwind(M.empty)

    class CannotZipException(left: Seq[Any], right: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\tleft:  $left\n\tright: $right")

    class CannotZip3Exception(seq1: Seq[Any], seq2: Seq[Any], seq3: Seq[Any]) extends Exception(
      s"Cannot zip because of different length:\n\t1: $seq1\n\t2: $seq2\n\t3: $seq3")
  }

  implicit class TxZipOps[F[x] <: Seq[x]: Monad: Traverse, A](tx: Tx[F, A]) {
    /** Will throw [[CannotZipException]] if lengths of the results do not correspond */
    def zip[B](that: Tx[F, B]): Tx[F, (A, B)] = Op.zip(tx, that)
  }


  final def query[F[_]]: QueryBuilder[F] = QueryBuilder.asInstanceOf[QueryBuilder[F]]

  protected[cypher] class QueryBuilder[F[_]] {
    def apply[A](query: Known[CF.Query[A]])(implicit M: Monad[F], T: Traverse[F], readTo: ReadTo[F], read: Reader[A]): Tx[F, read.Out] = Op.query(query)

    def apply[Params <: HList, A](query: Parameterized.Prepared[Params, CF.Query[A]])
                                 (implicit M: Monad[F], T: Traverse[F], readTo: ReadTo[F], reader: Reader[A], supported: SupportedParams[Params])
                                 : ParameterizedQueryBuilder[Params, F, A, reader.Out] =
      new ParameterizedQueryBuilder(query)(reader, readTo, supported)
  }
  private object QueryBuilder extends QueryBuilder[Id]

  final class ParameterizedQueryBuilder[Params <: HList, F[_], A, R]
      (q: Parameterized.Prepared[Params, CF.Query[A]])
      (implicit val reader: ReaderAux[A, R], readTo: ReadTo[F], supported: SupportedParams[Params])
    extends RecordArgs
  {
    def withParamsRecord(params: Params): Tx[F, R] = FreeT.liftF {
      import supported._
      val statement = q.changeParams[Poly](mapper)
                       .applyRecord(mapper(params))(toMap)
      PreparedQuery(statement, readTo, reader)
    }
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

}*/
