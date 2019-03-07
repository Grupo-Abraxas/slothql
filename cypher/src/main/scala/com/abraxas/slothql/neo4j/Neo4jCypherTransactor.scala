package com.abraxas.slothql.neo4j

import scala.collection.convert.decorateAsScala._
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.runtime.{ universe => ru }

import cats.effect.IO
import cats.instances.vector._
import cats.{ Functor, ~> }
import org.neo4j.driver.internal.types.InternalTypeSystem
import org.neo4j.driver.v1._
import shapeless.{ :: => #:, _ }

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
          tx.foldMap(Neo4jCypherTransactor.syncInterpreter[Vector](tx0, transaction))
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

    implicit def recordCellsReader[A](implicit reader: ValuesReader[A]): RecordReader.Aux[A, reader.Res] =
      RecordReader.define{ vs =>
        val (read, leftover) = reader(vs.values().asScala.toList)
        if (leftover.nonEmpty) sys.error(s"Leftover cells: $leftover")
        read
      }
  }

  trait ValuesReader[A] extends CypherTransactor.Reader[List[Value], A] {
    type Res
    type Out = (Res, List[Value])
  }
  object ValuesReader {
    type Aux[A, R] = ValuesReader[A] { type Res = R }

    def define[A, R](f: List[Value] => (R, List[Value])): Aux[A, R] =
      new ValuesReader[A] {
        type Res = R
        def apply(rec: List[Value]): (R, List[Value]) = f(rec)
      }


    implicit def singleValue[A](implicit vr: ValueReader[A]): ValuesReader.Aux[A, A] =
      ValuesReader define {
        case head :: tail => vr(head) -> tail
        case Nil => sys.error("Input ended unexpectedly")
      }

    // converts HList to tuple
    implicit def hlist[L <: HList, VL <: HList, ReadU, ReadRev <: HList, Read <: HList](
      implicit
      valueTypes: ops.hlist.LiftAll.Aux[HListImpl.ValueType, L, VL],
      fold: ops.hlist.LeftFolder.Aux[VL, (HNil, List[Value]), HListImpl.ReadValues.type, ReadU],
      unpack: Unpack2[ReadU, Tuple2, ReadRev, List[Value]],
      revRead: ops.hlist.Reverse.Aux[ReadRev, Read],
      toTuple: ops.hlist.Tupler[Read]
    ): ValuesReader.Aux[L, toTuple.Out] =
      ValuesReader define { cells =>
        val (read, rest) = fold(valueTypes.instances, HNil -> cells).asInstanceOf[(ReadRev, List[Value])]
        toTuple(revRead(read)) -> rest
      }


    object HListImpl {
      sealed trait ValueType[A]
      object ValueType {
        implicit def apply[A]: ValueType[A] = instance.asInstanceOf[ValueType[A]]
        private val instance = new ValueType[Any] {}
      }

      object ReadValues extends Poly2 {
        implicit def default[AccRev <: HList, A, R](
          implicit reader: Strict[ValuesReader.Aux[A, R]]
        ): Case.Aux[(AccRev, List[Value]), ValueType[A], (R #: AccRev, List[Value])] =
          at { case ((accRev, values), _) =>
            val (read, rest) = reader.value(values)
            (read :: accRev) -> rest
          }

        implicit def product[AccRev <: HList, A, Repr <: HList, R](
          implicit gen: Generic.Aux[A, Repr], reader: ValuesReader.Aux[Repr, R]
        ): Case.Aux[(AccRev, List[Value]), ValueType[A], (R #: AccRev, List[Value])] =
          at { case ((accRev, values), _) =>
            val (read, rest) = reader(values)
            (read :: accRev) -> rest
          }
      }
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
      ValueReader define (s"Option[${reader.describeResult}]", ifNotNull(reader.apply _ andThen Some.apply, None))

    implicit def list[A](implicit reader: ValueReader[A]): ValueReader[List[A]] =
      ValueReader define (s"List[${reader.describeResult}]", ifNotNull(_.values(reader.apply(_: Value)).asScala.toList, Nil) )

    implicit def map[A](implicit reader: ValueReader[A]): ValueReader[Map[String, A]] =
      ValueReader define (s"Map[String, ${reader.describeResult}]", ifNotNull(_.asMap(reader.apply(_: Value)).asScala.toMap, Map()))

    private def ifNotNull[R](notNull: Value => R, isNull: => R): Value => R = v => if (v.isNull) isNull else notNull(v)

    lazy val cell: ValueReader[Cell] = ValueReader define ("Cell", locally)

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
    implicit lazy val bigInt: ValueReader[BigInt] = ValueReader define ("BigInt", BigInt apply _.asString())
    implicit lazy val bigDecimal: ValueReader[BigDecimal] = ValueReader define ("BigDecimal", BigDecimal apply _.asString())

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
        ru.typeOf[String]  -> string,
        ru.typeOf[Boolean] -> boolean,
        ru.typeOf[Int]     -> int,
        ru.typeOf[Long]    -> long,
        ru.typeOf[Float]   -> float,
        ru.typeOf[Double]  -> double,
        ru.typeOf[BigInt]  -> bigInt,
        ru.typeOf[BigDecimal] -> bigDecimal
      )

      private val OptionType    = ru.typeOf[Option[_]]
      private val ListType      = ru.typeOf[List[_]]
      private val StringMapType = ru.typeOf[Map[String, _]]
    }
  }

  implicit class UntypedListCellsOps(ul: CypherFragment.Return.Untyped) {
    def toCells: CypherFragment.Return.Return0[List[Cell]] = ul.asInstanceOf[CypherFragment.Return.Return0[List[Cell]]]
  }

  protected def syncInterpreter[F[_] <: Iterable[_]](t: Neo4jCypherTransactor, tx: Transaction)
                                                    (implicit cbf: CanBuildFrom[Nothing, Any, F[_]]): t.Read ~> F =
    λ[t.Read ~> F]{
      case    t.txBuilder.Unwind(i)       => fromIterable(i)
      case g@ t.txBuilder.Gather(r)       => fromIterable(Seq(g.fromIterable(syncInterpreter(t, tx)(cbf)(r))))
      case rq@t.txBuilder.ReadQuery(_, _) => fromIterable(runReadQueryTxSync(tx, rq))
    }

  protected def runReadQueryTxSync[R](tx: Transaction, r: ReadQuery[_, R]): Iterable[R] =
    tx.run(new Statement(r.query.toCypher)).list(r.reader(_: Record)).asScala // TODO: issue #8


  private def fromIterable[F[_], A](a: Iterable[A])(implicit cbf: CanBuildFrom[Nothing, Any, F[_]]): F[A] =
    (cbf.apply() ++= a).result().asInstanceOf[F[A]]
  }