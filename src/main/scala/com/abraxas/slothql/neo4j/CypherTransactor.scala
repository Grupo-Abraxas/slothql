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

    implicit lazy val any: Aux[Any, Map[String, AnyRef]] =
      RecordReader define (_.asMap().asScala.toMap)
    implicit lazy val anyList: Aux[List[Any], List[AnyRef]] =
      RecordReader define (_.values().asScala.map(_.asObject()).toList)

    private object ReadValue extends Poly2 {
      implicit def impl[A, R]: Case.Aux[ValueReader.Aux[A, R], Value, R] = at[ValueReader.Aux[A, R], Value](_ apply _)
    }
    implicit def tuple[T <: Product, Repr <: HList, ReprReaders <: HList, Values <: HList, Read <: HList, ReadT <: Product](
      implicit
      gen: Generic.Aux[T, Repr],
      lift: ops.hlist.LiftAll.Aux[ValueReader, Repr, ReprReaders],
      values: ops.traversable.FromTraversable[Values],
      zipApply: ops.hlist.ZipWith.Aux[ReprReaders, Values, ReadValue.type, Read],
      toTuple: ops.hlist.Tupler.Aux[Read, ReadT]
    ): Aux[T, ReadT] =
      RecordReader define { record =>
        val Some(vs) = values(record.values().asScala)
        toTuple(zipApply(lift.instances, vs))
      }

  }

  type ValueReader[A] = Reader[Value, A]
  object ValueReader {
    type Aux[A, R] = Reader.Aux[Value, A, R]
  }

}