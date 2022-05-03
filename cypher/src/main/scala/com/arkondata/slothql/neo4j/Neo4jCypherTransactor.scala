package com.arkondata.slothql.neo4j

import scala.annotation.implicitNotFound
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters.ScalaDurationOps

import cats.arrow.{ Arrow, FunctionK }
import cats.data.StateT
import cats.effect.Resource
import cats.effect.kernel.{ Async, Deferred }
import cats.effect.std.Dispatcher
import cats.instances.function._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ ~>, Applicative, Monad }
import fs2.interop.reactivestreams._
import org.neo4j.driver.internal.types.InternalTypeSystem
import org.neo4j.driver.reactive.{ RxSession, RxTransaction, RxTransactionWork }
import org.neo4j.driver.types.{ Node => NNode, Path => NPath, Relationship => NRelationship, Type }
import org.neo4j.driver.{ Driver, Record, TransactionConfig, Value }
import org.reactivestreams.Publisher
import shapeless._

import com.arkondata.slothql.cypher
import com.arkondata.slothql.cypher.CypherTransactor
import com.arkondata.slothql.cypher.CypherTransactor._
import com.arkondata.slothql.neo4j.util.fs2StreamTxCMonad

class Neo4jCypherTransactor[F[_]](
  driver: Driver,
  completion: Deferred[F, Unit],
  defaultTimeout: FiniteDuration,
  chunkSize: Int
)(implicit
  dispatcher: Dispatcher[F],
  F: Async[F]
) extends Neo4jCypherTransactor.Syntax[F]
    with CypherTransactor[F, Record, fs2.Stream[F, *]] {

  object readers extends Neo4jCypherTransactor.Readers

  override type Tx[R] = CypherTransactor.Tx[F, Record, fs2.Stream[F, *], R]

  override type Out[R] = fs2.Stream[F, R]

  private type TxS[R] = CypherTransactor.Tx[fs2.Stream[F, *], Record, fs2.Stream[F, *], R]

  private type OpS[R] = Operation[Record, fs2.Stream[F, *], R]

  private object Tx {

    // Tx[A] ~> TxS[A]
    def streamK: F ~> fs2.Stream[F, *] = new ~>[F, fs2.Stream[F, *]] {
      override def apply[A](fa: F[A]): fs2.Stream[F, A] = fs2.Stream.eval(fa)
    }

    def runOp(transactor: RxTransaction): OpS ~> fs2.Stream[F, *] =
      new ~>[OpS, fs2.Stream[F, *]] {

        override def apply[A](fa: OpS[A]): fs2.Stream[F, A] = fa match {
          case CypherTransactor.Unwind(values) => values
          case CypherTransactor.Query(query, read) =>
            transactor
              .run(query.template, query.params.asJava)
              .records()
              .toStreamBuffered(chunkSize)
              .evalMap(r => F.delay(read(r)))
          case CypherTransactor.Gather(value, fn) => fs2.Stream.emit(fn(apply(value)))
        }
      }
  }

  override def runRead[R](tx: Tx[R]): fs2.Stream[F, R] =
    apply(tx, defaultTimeout, write = false)

  override def runWrite[R](tx: Tx[R]): fs2.Stream[F, R] =
    apply(tx, defaultTimeout, write = true)

  @inline def runRead[R](tx: Tx[R], timeout: FiniteDuration): fs2.Stream[F, R] =
    apply(tx, timeout, write = false)

  @inline def runWrite[R](tx: Tx[R], timeout: FiniteDuration): fs2.Stream[F, R] =
    apply(tx, timeout, write = true)

  def apply[R](tx: Tx[R], timeout: FiniteDuration, write: Boolean): fs2.Stream[F, R] =
    unsafeSyncStream(tx.mapK(Tx.streamK), timeout * 2, write)

  private def unsafeSyncStream[R](txs: TxS[R], timeout: FiniteDuration, write: Boolean): fs2.Stream[F, R] =
    fs2.Stream
      .resource(sessionResource)
      .flatMap(
        sessionFn[R](_, write)(
          tx => StreamUnicastPublisher(txs.foldMap(Tx.runOp(tx)), dispatcher),
          TransactionConfig.builder().withTimeout(timeout.toJava).build()
        ).toStreamBuffered(chunkSize)
      )

  private def sessionFn[A](
    session: RxSession,
    write: Boolean
  ): (RxTransactionWork[Publisher[A]], TransactionConfig) => Publisher[A] = (fn, cfg) =>
    if (write) session.writeTransaction(fn, cfg) else session.readTransaction(fn, cfg)

  private lazy val sessionResource: Resource[F, RxSession] = Resource.makeCase(
    completion.tryGet.map(_.isDefined).flatMap(F.raiseError(new IllegalStateException("Driver is closed")).whenA) *>
    F.delay(driver.rxSession())
  )((s, _) => s.close().toStreamBuffered(chunkSize).compile.drain)

}

object Neo4jCypherTransactor {
  type Tx[F[_], R] = CypherTransactor.Tx[F, Record, fs2.Stream[F, *], R]

  def apply[F[_]: Async](
    driver: Driver,
    defaultTimeout: FiniteDuration = 10.seconds,
    chunkSize: Int = 1024
  )(implicit
    dispatcher: Dispatcher[F]
  ): F[(Neo4jCypherTransactor[F], Deferred[F, Unit])] =
    Deferred[F, Unit].map(defer => (new Neo4jCypherTransactor[F](driver, defer, defaultTimeout, chunkSize), defer))

  def imapK[F[_], G[_]: Monad](f: F ~> G, g: G ~> F): Tx[F, *] ~> Tx[G, *] =
    λ[Tx[F, *] ~> Tx[G, *]](
      _.mapK(f)
        .compile(
          λ[Operation[Record, fs2.Stream[F, *], *] ~> Operation[Record, fs2.Stream[G, *], *]](
            _.imapK(
              λ[fs2.Stream[F, *] ~> fs2.Stream[G, *]](_.translate(f)),
              λ[fs2.Stream[G, *] ~> fs2.Stream[F, *]](_.translate(g))
            )
          )
        )
    )

  // // // // // // // // //
  // // //  Readers // // //
  // // // // // // // // //

  trait ValueReader[A] extends Reader[Value, A] {
    final def sourceName: String = "Value"
    def name: String
  }

  object ValueReader {

    def apply[A](nme: String, f: Value => A): ValueReader[A] =
      new ValueReader[A] {
        def name: String         = nme
        def apply(src: Value): A = f(src)
      }
  }

  final case class RootReader[A](name: String, read: StateT[cats.Id, Seq[Value], A]) extends Reader[Seq[Value], A] {
    def sourceName: String = "Seq[Value]"

    def apply(values: Seq[Value]): A = {
      val (rest, result) = read.run(values)
      if (rest.nonEmpty) sys.error(s"Failed to read $name. Values remained: $rest")
      result
    }
  }

  object RootReader {

    implicit def singleReader[A](implicit read: ValueReader[A]): RootReader[A] =
      RootReader(read.name, StateT { case h +: t => t -> read(h) })

    implicit def productReader[T <: Product, Repr <: HList](implicit
      gen: Generic.Aux[T, Repr],
      reader: ProductReader[Repr],
      lowPriority: LowPriority
    ): RootReader[T] = {
      val name = reader.names.mkString("(", ",", ")")
      RootReader[T](name, StateT[cats.Id, Seq[Value], T](reader.apply _ andThen Arrow[Function1].second(gen.from)))
    }

    @implicitNotFound("Cannot read product ${T}")
    trait ProductReader[T <: HList] {
      type Values = Seq[Value]
      val names: List[String]
      def apply(src: Values): (Values, T)
    }

    object ProductReader {

      implicit lazy val hnilReader: ProductReader[HNil] =
        new ProductReader[HNil] {
          val names: List[String]                = Nil
          def apply(src: Values): (Values, HNil) = (src, HNil)
        }

      implicit def hconsReader[H, T <: HList](implicit
        headReader: RootReader[H],
        tailReader: ProductReader[T]
      ): ProductReader[H :: T] =
        new ProductReader[H :: T] {
          val names: List[String] = headReader.name :: tailReader.names

          def apply(src: Values): (Values, H :: T) = {
            val (next, head) = headReader.read.run(src)
            val (rest, tail) = tailReader(next)
            (rest, head :: tail)
          }
        }
    }
  }

  trait AnyValueReader {

    def readValue(v: Value): AnyRef =
      knownValues
        .get(v.`type`())
        .map(_(v).asInstanceOf[AnyRef])
        .getOrElse(v.asObject())

    protected val knownValues: Map[Type, Reader[Value, _]]
  }

  trait DefaultValueReaders extends AnyValueReader {
    protected val Type = InternalTypeSystem.TYPE_SYSTEM

    protected val knownValues: Map[Type, Reader[Value, _]] = Map(
      Type.STRING()       -> defaultNeo4jStringReader,
      Type.INTEGER()      -> defaultNeo4jLongReader,
      Type.FLOAT()        -> defaultNeo4jDoubleReader,
      Type.BOOLEAN()      -> defaultNeo4jBooleanReader,
      Type.BYTES()        -> defaultNeo4jBytesReader,
      Type.NULL()         -> ValueReader("null", _ => None), // `null` wouldn't cause class cast exception
      Type.LIST()         -> defaultNeo4jListReader(defaultNeo4jAnyReader),
      Type.MAP()          -> defaultNeo4jMapReader(defaultNeo4jAnyReader),
      Type.NODE()         -> defaultNeo4jNodeReader,
      Type.RELATIONSHIP() -> defaultNeo4jRelReader,
      Type.PATH()         -> defaultNeo4jPathReader
    )

    implicit lazy val defaultNeo4jStringReader: ValueReader[String]     = ValueReader("String", _.asString())
    implicit lazy val defaultNeo4jIntReader: ValueReader[Int]           = ValueReader("Int", _.asInt())
    implicit lazy val defaultNeo4jLongReader: ValueReader[Long]         = ValueReader("Long", _.asLong())
    implicit lazy val defaultNeo4jFloatReader: ValueReader[Float]       = ValueReader("Float", _.asFloat())
    implicit lazy val defaultNeo4jDoubleReader: ValueReader[Double]     = ValueReader("Double", _.asDouble())
    implicit lazy val defaultNeo4jBooleanReader: ValueReader[Boolean]   = ValueReader("Boolean", _.asBoolean())
    implicit lazy val defaultNeo4jBytesReader: ValueReader[Array[Byte]] = ValueReader("Array[Byte]", _.asByteArray())
    implicit lazy val defaultNeo4jAnyRefReader: ValueReader[AnyRef]     = ValueReader("AnyRef", readValue)
    implicit lazy val defaultNeo4jAnyReader: ValueReader[Any]           = ValueReader("Any", readValue)

    implicit lazy val defaultNeo4jBigDecReader: ValueReader[BigDecimal] =
      ValueReader("BigDecimal", BigDecimal apply _.asString())
    implicit lazy val defaultNeo4jBigIntReader: ValueReader[BigInt] = ValueReader("BigInt", BigInt apply _.asString())

    implicit lazy val defaultNeo4jNodeReader: ValueReader[cypher.GraphElem.Node] =
      ValueReader("Node", v => mkNode(v.asNode()))

    implicit lazy val defaultNeo4jRelReader: ValueReader[cypher.GraphElem.Rel] =
      ValueReader("Rel", v => mkRel(v.asRelationship()))

    implicit lazy val defaultNeo4jPathReader: ValueReader[cypher.GraphPath] =
      ValueReader("Path", v => mkPath(v.asPath()))

    implicit def defaultNeo4jOptionReader[A](implicit read: ValueReader[A]): ValueReader[Option[A]] =
      ValueReader(s"Option[${read.name}]", v => Option.when(!v.isNull)(read(v)))

    implicit def defaultNeo4jListReader[A](implicit read: ValueReader[A]): ValueReader[List[A]] =
      ValueReader(s"List[${read.name}]", _.asList(read(_)).asScala.toList)

    implicit def defaultNeo4jMapReader[A](implicit read: ValueReader[A]): ValueReader[Map[String, A]] =
      ValueReader(s"Map[String, ${read.name}]", _.asMap(read(_)).asScala.toMap)

    private def mkNode(node: NNode) =
      cypher.GraphElem.Node(
        node.id(),
        node.labels().asScala.toList,
        node.asMap(readValue).asScala.toMap
      )

    private def mkRel(rel: NRelationship) =
      cypher.GraphElem.Rel(
        rel.id(),
        rel.`type`(),
        rel.startNodeId(),
        rel.endNodeId(),
        rel.asMap(readValue).asScala.toMap
      )

    private def mkPath(path: NPath) =
      cypher.GraphPath(
        path.nodes().asScala.toList.map(mkNode),
        path.relationships().asScala.toList.map(mkRel)
      )
  }

  implicit def neo4jRecordReader[A](implicit r: RootReader[A]): CypherTransactor.Reader[Record, A] =
    new CypherTransactor.Reader[Record, A] {
      def sourceName: String    = "Record"
      def name: String          = r.name
      def apply(rec: Record): A = r(rec.values().asScala.toSeq)
    }

  trait Readers extends DefaultValueReaders {

    implicit def neo4jRecordReader[A](implicit r: RootReader[A]): CypherTransactor.Reader[Record, A] =
      Neo4jCypherTransactor.neo4jRecordReader[A]
  }

  object Readers extends Readers

  // // // // // // // // //
  // // //  Syntax  // // //
  // // // // // // // // //

  class Syntax[F[_]: Applicative](implicit compiler: fs2.Compiler[F, F])
      extends CypherTransactor.Syntax[F, Record, fs2.Stream[F, *]] {
    syntax =>

    type TxC[A]       = CypherTransactor.TxC[F, Record, fs2.Stream[F, *], A]
    type TxG[G[_], A] = CypherTransactor.Tx[F, Record, fs2.Stream[F, *], G[A]]

    def gatherStream[G[_]](to: fs2.Stream.CompileOps[F, F, *] ~> λ[A => F[G[A]]]): Tx ~> λ[A => Tx[G[A]]] =
      gather.andThen[TxG[G, *]](λ[TxC ~> TxG[G, *]](_.flatMap(s => liftF(to(s.compile)))))

    override val ops: Neo4jOps = new Neo4jOps

    protected class Neo4jOps extends Ops {

      implicit final class SyntaxGatherStreamOps[A](tx: Tx[A]) {

        def gatherStream[C[_]](to: fs2.Stream.CompileOps[F, F, A] => F[C[A]]): Tx[C[A]] = {
          def func[X](ops: fs2.Stream.CompileOps[F, F, X]): F[C[X]] =
            to.asInstanceOf[fs2.Stream.CompileOps[F, F, X] => F[C[X]]](ops)
          val funcK = FunctionK.lift[fs2.Stream.CompileOps[F, F, *], λ[A => F[C[A]]]](func)
          syntax.gatherStream(funcK)(tx)
        }
      }
    }

    final protected def drainC[R]: CypherTransactor.TxC[F, Record, fs2.Stream[F, *], R] => Tx[Unit] =
      _.flatMap(s => liftF(s.compile.drain))
  }

}
