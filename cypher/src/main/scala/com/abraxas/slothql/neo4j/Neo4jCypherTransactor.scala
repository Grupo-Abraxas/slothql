package com.abraxas.slothql.neo4j

import scala.collection.convert.decorateAsScala._
import scala.reflect.runtime.{ universe => ru }

import cats.effect.IO
import cats.instances.vector._
import cats.{ Functor, ~> }
import org.neo4j.driver.internal.types.InternalTypeSystem
import org.neo4j.driver.v1._
import shapeless._

import com.abraxas.slothql.cypher.{ CypherFragment, CypherTransactor, CypherTxBuilder }
import com.abraxas.slothql.neo4j.util.JavaExt._


class Neo4jCypherTransactor(protected val session: () => Session) extends CypherTransactor {
  tx0 =>

  type TxBuilder = Neo4jCypherTransactor.type
  val txBuilder = Neo4jCypherTransactor

  import Neo4jCypherTransactor._

  def runRead[A](tx: ReadTx[A]): IO[Seq[A]] =
    IO {
      session().readTransaction(new TransactionWork[Seq[A]] {
        def execute(transaction: Transaction): Seq[A] =
          tx.foldMap(Neo4jCypherTransactor.syncInterpreter(tx0, transaction))
      })
    }
  def runWrite[A](tx: WriteTx[A]): IO[Seq[A]] = ??? // TODO
}

object Neo4jCypherTransactor extends CypherTxBuilder {
  type Result = Record
  type Reader[A] = RecordReader[A]

  type Cell = Value

  def apply(session: => Session): Neo4jCypherTransactor = new Neo4jCypherTransactor(() => session)

  trait RecordReader[A] extends CypherTransactor.Reader[Record, A]
  object RecordReader {
    type Aux[A, R] = RecordReader[A] { type Out = R }

    def define[A, R](f: Record => R): Aux[A, R] =
      new RecordReader[A] {
        type Out = R
        def apply(rec: Record): R = f(rec)
      }

    implicit def resultCells: RecordReader.Aux[List[Cell], List[Cell]] = RecordReader define { _.values().asScala.toList }

    implicit def singleValue[A](implicit vr: ValueReader[A], lowPriority: LowPriority): RecordReader.Aux[A, A] =
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
    ): RecordReader.Aux[L, toTuple.Out] =
      RecordReader define { record =>
        val Some(vs) = values(record.values().asScala)
        toTuple(zipApply(stubL(), vs))
      }

  }

  trait ValueReader[A] extends CypherTransactor.Reader[Value, A] {
    type Out = A
    def describeResult: String

    def map[B](describe: String, f: A => B): ValueReader[B] = ValueReader.define(describe, f compose apply)
    def map[B](f: A => B): ValueReader[B] = map(s"$describeResult ~> ?", f)

    override def toString: String = s"ValueReader[$describeResult]"
  }
  object ValueReader {
    def define[A](describe: String, f: Value => A): ValueReader[A] =
      new ValueReader[A] {
        def apply(rec: Value): A = f(rec)
        def describeResult: String = describe
      }

    implicit lazy val ValueReaderFunctor: Functor[ValueReader] =
      new Functor[ValueReader] {
        def map[A, B](fa: ValueReader[A])(f: A => B): ValueReader[B] = fa.map(f)
      }

    implicit lazy val ValueIsTypeable: Typeable[Value] = Typeable.simpleTypeable(classOf[Value])

    implicit def option[A](implicit reader: ValueReader[A]): ValueReader[Option[A]] =
      ValueReader define (s"Option[${reader.describeResult}]", v => if (v.isNull) None else Some(reader(v)))

    implicit def list[A](implicit reader: ValueReader[A]): ValueReader[List[A]] =
      ValueReader define (s"List[${reader.describeResult}]", _.values(reader.apply(_: Value)).asScala.toList )

    implicit def map[A](implicit reader: ValueReader[A]): ValueReader[Map[String, A]] =
      ValueReader define (s"Map[String, ${reader.describeResult}]", _.asMap(reader.apply(_: Value)).asScala.toMap)

    implicit lazy val any: ValueReader[Any] = ValueReader define ("Any", {
      case v if v.hasType(InternalTypeSystem.TYPE_SYSTEM.LIST) => list[Any].apply(v)
      case v if v.hasType(InternalTypeSystem.TYPE_SYSTEM.MAP)  => map[Any].apply(v)
      case v if v.isNull => None
      case v             => v.asObject()
    })
    implicit lazy val boolean: ValueReader[Boolean] = ValueReader define ("Boolean", _.asBoolean())
    implicit lazy val string: ValueReader[String] = ValueReader define ("String", _.asString())

    implicit lazy val int: ValueReader[Int] = ValueReader define ("Int", _.asInt())
    implicit lazy val long: ValueReader[Long] = ValueReader define ("Long", _.asLong())
    implicit lazy val float: ValueReader[Float] = ValueReader define ("Float", _.asFloat())
    implicit lazy val double: ValueReader[Double] = ValueReader define ("Double", _.asDouble())

    implicit lazy val cell: ValueReader[Cell] = ValueReader define ("Cell", locally)

    object Default extends Default
    class Default {
      def apply[A: ru.TypeTag]: ValueReader[_] = apply(ru.typeOf[A])
      def apply(tpe: ru.Type): ValueReader[_] = {
        lazy val firstTArgReader = apply(tpe.typeArgs.head)
        lazy val secondTArgReader = apply(tpe.typeArgs(1))
        tpe match {
          case _ if tpe <:< OptionType    => ValueReader.option(firstTArgReader)
          case _ if tpe <:< ListType      => ValueReader.list(firstTArgReader)
          case _ if tpe <:< StringMapType => ValueReader.map(secondTArgReader)
          case _ =>
            val readers = atomicReaders.withFilter(_._1 <:< tpe).map(_._2)
            readers.size match {
              case 0 => sys.error(s"No default ValueReader is defined for $tpe")
              case 1 => readers.head
              case _ => sys.error(s"Multiple ValueReaders are defined for $tpe: ${readers.mkString(", ")}")
            }
        }
      }

      protected def atomicReaders = Map[ru.Type, ValueReader[_]](
        ru.typeOf[Any]     -> any,
        ru.typeOf[Cell]    -> cell,
        ru.typeOf[String]  -> string,
        ru.typeOf[Boolean] -> boolean,
        ru.typeOf[Int]     -> int,
        ru.typeOf[Long]    -> long,
        ru.typeOf[Float]   -> float,
        ru.typeOf[Double]  -> double
      )

      private val OptionType    = ru.typeOf[Option[_]]
      private val ListType      = ru.typeOf[List[_]]
      private val StringMapType = ru.typeOf[Map[String, _]]
    }
  }

  implicit class UntypedListCellsOps(ul: CypherFragment.Return.UntypedList) {
    def toCells: CypherFragment.Return.Return0[List[Cell]] = ul.asInstanceOf[CypherFragment.Return.Return0[List[Cell]]]
  }

  protected def syncInterpreter(t: Neo4jCypherTransactor, tx: Transaction): t.Read ~> Vector =
    λ[t.Read ~> Vector]{
      case t.txBuilder.Unwind(i) => i.toVector
      case t.txBuilder.Gather(r) => Vector(syncInterpreter(t, tx)(r))
      case r                     => runReadTxSync(tx, r)
    }

  protected def runReadTxSync[A](tx: Transaction, r: Read[A]): Vector[A] =
    tx.run(new Statement(r.query.toCypher)).list(r.reader(_: Record)).asScala.toVector // TODO: issue #8

}