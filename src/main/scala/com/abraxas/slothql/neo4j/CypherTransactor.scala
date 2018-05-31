package com.abraxas.slothql.neo4j

import scala.collection.convert.decorateAsScala._

import cats.effect.IO
import org.neo4j.driver.v1._
import shapeless._

import com.abraxas.slothql.cypher.CypherFragment._
import com.abraxas.slothql.neo4j.util.JavaExt._

trait CypherTransactor {
  type Result
  def read[A](query: Known[Query[A]])(implicit read: CypherTransactor.Reader[Result, A]): IO[Seq[read.Out]]
}

object CypherTransactor {
  type Aux[R] = CypherTransactor { type Result = R }

  trait Reader[Src, A] extends DepFn1[Src]
  object Reader {
    type Aux[Src, A, R] = Reader[Src, A] { type Out = R }
    def apply[Src, A](implicit reader: Reader[Src, A]): Aux[Src, A, reader.Out] = reader
  }

  class Default(protected val session: () => Session) extends CypherTransactor {
    type Result = Record
    def read[A](query: Known[Query[A]])(implicit read: Reader[Result, A]): IO[Seq[read.Out]] =
      IO {
        session().readTransaction(new TransactionWork[Seq[read.Out]] {
          def execute(tx: Transaction): Seq[read.Out] = tx.run(new Statement(query.toCypher)).list(read(_: Record)).asScala // TODO
        })
      }
  }
  object Default {
    def apply(session: => Session): Default = new Default(() => session)
  }


  type RecordReader[A] = Reader[Record, A]
  object RecordReader {
    type Aux[A, R] = Reader.Aux[Record, A, R]

    def define[A, R](f: Record => R): Aux[A, R] =
      new Reader[Record, A] {
        type Out = R
        def apply(rec: Record): R = f(rec)
      }


    implicit def singleValue[A](implicit vr: ValueReader[A]): Aux[A, A] =
      RecordReader define { rec =>
        vr(rec.ensuring(_.size() == 1).values().get(0))
      }

    private object ReadValue extends Poly2 {
      implicit def impl[A](implicit reader: ValueReader[A]): Case.Aux[A, Value, A] =
        at[A, Value]((_, v) => reader(v))
    }

    private object Null extends Poly0 {
      implicit def impl[A]: Case0[A] = at[A](null.asInstanceOf[A])
    }

    // converts HList to tuple
    implicit def hlist[L <: HList, Values <: HList, Read <: HList](
      implicit
      stubL: ops.hlist.FillWith[Null.type, L],
      valuesT: ops.hlist.ConstMapper.Aux[Value, L, Values],
      values: ops.traversable.FromTraversable[Values],
      zipApply: ops.hlist.ZipWith.Aux[L, Values, ReadValue.type, Read],
      toTuple: ops.hlist.Tupler[Read]
    ): Aux[L, toTuple.Out] =
      RecordReader define { record =>
        val Some(vs) = values(record.values().asScala)
        toTuple(zipApply(stubL(), vs))
      }

  }

  type ValueReader[A] = Reader.Aux[Value, A, A]
  object ValueReader {
    def define[A](f: Value => A): ValueReader[A] =
      new Reader[Value, A] {
        type Out = A
        def apply(rec: Value): A = f(rec)
      }

    implicit lazy val ValueIsTypeable: Typeable[Value] = Typeable.simpleTypeable(classOf[Value])

    implicit def option[A](implicit reader: ValueReader[A]): ValueReader[Option[A]] = ValueReader define { v =>
      if (v.isNull) None else Some(reader(v))
    }

    implicit def list[A](implicit reader: ValueReader[A]): ValueReader[List[A]] =
      ValueReader define (_.values(reader.apply(_: Value)).asScala.toList )

    implicit def map[A](implicit reader: ValueReader[A]): ValueReader[Map[String, A]] =
      ValueReader define (_.asMap(reader.apply(_: Value)).asScala.toMap)

    implicit lazy val any: ValueReader[Any] = ValueReader define (_.asObject())
    implicit lazy val boolean: ValueReader[Boolean] = ValueReader define (_.asBoolean())
    implicit lazy val string: ValueReader[String] = ValueReader define (_.asString())

    implicit lazy val int: ValueReader[Int] = ValueReader define (_.asInt())
    implicit lazy val long: ValueReader[Long] = ValueReader define (_.asLong())
    implicit lazy val float: ValueReader[Float] = ValueReader define (_.asFloat())
    implicit lazy val double: ValueReader[Double] = ValueReader define (_.asDouble())

  }

}