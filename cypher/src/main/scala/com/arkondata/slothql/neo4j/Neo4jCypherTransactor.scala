package com.arkondata.slothql.neo4j

import scala.annotation.implicitNotFound
import scala.jdk.CollectionConverters._
import scala.language.existentials

import cats.{ Applicative, Monad, StackSafeMonad, ~> }
import cats.arrow.{ Arrow, FunctionK }
import cats.data.StateT
import cats.effect.{ Blocker, Concurrent, ConcurrentEffect, ContextShift, ExitCase, Resource, Sync }
import cats.effect.concurrent.MVar
import cats.effect.syntax.bracket._
import cats.effect.syntax.effect._
import cats.instances.function._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.syntax.parallel._
import org.neo4j.driver.{ Driver, Record, Result, Session, Transaction, TransactionWork, Value }
import org.neo4j.driver.internal.types.InternalTypeSystem
import org.neo4j.driver.types.{ Type, Node => NNode, Path => NPath, Relationship => NRelationship }
import shapeless._

import com.arkondata.slothql.cypher
import com.arkondata.slothql.cypher.{ CypherStatement, CypherTransactor }
import com.arkondata.slothql.cypher.CypherTransactor._
import com.arkondata.slothql.neo4j.util.{ fs2StreamTxCMonad, javaStreamToFs2 }

class Neo4jCypherTransactor[F[_]](protected val session: F[Session])
                                (implicit protected val ce: ConcurrentEffect[F],
                                 implicit protected val cs: ContextShift[F])
  extends Neo4jCypherTransactor.Syntax[F]
     with CypherTransactor[F, Record, fs2.Stream[F, *]]
{
  import ce.delay

  override type Tx[R] = CypherTransactor.Tx [F, Record, fs2.Stream[F, *], R]

  type Out[R] = fs2.Stream[F, R]
  type Op [R] = Operation[Record, Out, R]

  object readers extends Neo4jCypherTransactor.Readers

  def runRead [A](tx: Tx[A]): Out[A] = run(tx, _.readTransaction)
  def runWrite[A](tx: Tx[A]): Out[A] = run(tx, _.writeTransaction)

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  protected def run[A](tx: Tx[A], txWork0: Session => (TransactionWork[B] => B) forSome { type B }): Out[A] = {
    val txWork = txWork0.asInstanceOf[Session => TransactionWork[Unit] => Unit]
    def outT[R](fr: F[R]): OutT[R] = _ => fs2.Stream.eval(fr)
    val r: Resource[F, Out[A]] = for {
      session <- sessionResource
      blocker <- blockerResource
      exec = tx.mapK(FunctionK.lift(outT))
               .foldMap(λ[Op ~> OutT](runOperation(blocker, _)))
      tx <- transactionResource(txWork(session))
    } yield exec(tx)
    fs2.Stream.resource(r).flatten
  }

  protected def blockerResource: Resource[F, Blocker] = Blocker[F]

  protected def sessionResource: Resource[F, Session] = Resource.liftF(session)
  // TODO: `Resource.make(session)(s => delay(s.close()))` fails; maybe fs2.Stream.resourceWeak should be used at `run`?

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  protected def transactionResource(run: TransactionWork[Unit] => Unit): Resource[F, Transaction] =
    for {
      txVar <- transactionMVarResource
      cLock <- closeLockMVarResource
      eLock <- execLockMVarResource(cLock)
      runTx = delay(run{ tx => (
                          for {
                            _ <- txVar.put(tx)
                            b <- eLock.read
                            _ <- if (b) commitTransaction(tx) else rollbackTransaction(tx)
                          } yield ()
                        ).guarantee(closeTransaction(tx))
                         .guaranteeCase {
                           case ExitCase.Completed => cLock.put(None)
                           case ExitCase.Error(e)  => cLock.put(Some(e))
                           case ExitCase.Canceled  => cLock.put(Some(new Exception("Canceled")))
                         }.toIO
                          .unsafeRunSync()
                    })
      _     <- backgroundWorkResource(runTx)
      tx    <- readTxAsResource(txVar)
    } yield tx

  protected def transactionMVarResource: Resource[F, MVar[F, Transaction]] = Resource liftF MVar.empty[F, Transaction]

  protected def execLockMVarResource(lock1: MVar[F, Option[Throwable]]): Resource[F, MVar[F, Boolean]] = {
    def waitClose = lock1.read.map(_.toLeft(())).rethrow
    Resource.makeCase(MVar.empty[F, Boolean]) {
      case (v, ExitCase.Completed) => v.put(true)  >> waitClose
      case (v, _)                  => v.put(false) >> waitClose
    }
  }

  protected def closeLockMVarResource: Resource[F, MVar[F, Option[Throwable]]] = Resource.liftF(MVar.empty[F, Option[Throwable]])

  protected def commitTransaction(tx: Transaction): F[Unit]   = delay(tx.commit())
  protected def rollbackTransaction(tx: Transaction): F[Unit] = delay(tx.rollback())
  protected def closeTransaction(tx: Transaction): F[Unit]    = delay(tx.close())

  protected def backgroundWorkResource(work: F[Unit]): Resource[F, F[Unit]] = Concurrent[F].background(work)

  protected def readTxAsResource(txVar: MVar[F, Transaction]): Resource[F, Transaction] = Resource.liftF(txVar.read)

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  protected type OutT[R] = Transaction => Out[R]

  protected implicit lazy val outTMonad: Monad[OutT] = new Monad[OutT] with StackSafeMonad[OutT] {
    def pure[A](x: A): OutT[A] = _ => fs2.Stream.emit(x)
    def flatMap[A, B](fa: OutT[A])(f: A => OutT[B]): OutT[B] = tx => fa(tx).flatMap(f andThen (_(tx)))
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  protected def runOperation[A](blocker: Blocker, op: Op[A]): OutT[A] = op match {
    case Unwind(out)   => runUnwind(out)
    case Gather(op, f) => runGather(blocker, op, f)
    case Query(q, r)   => runQuery(blocker, q, r)
  }

  protected def runUnwind[A](out: Out[A])(tx: Transaction): Out[A] = out

  protected def runGather[A, B](blocker: Blocker, op: Op[B], gather: Out[B] => A)(tx: Transaction): Out[A] =
    fs2.Stream.emit(gather(runOperation(blocker, op)(tx)))

  protected def runQuery[A](blocker: Blocker, q: CypherStatement.Prepared[A], reader: Reader[A])(tx: Transaction): fs2.Stream[F, A] =
    fs2.Stream force runQuery0(tx, q).fproduct { result =>
      javaStreamToFs2(blocker, ce.delay{ result.stream() }).evalMap(readRecord(_, reader))
    }.map((runningQuery[A] _).tupled)

  protected def runQuery0(tx: Transaction, q: CypherStatement.Prepared[_]): F[Result] =
    delay { tx.run(q.template, q.params.asJava) }

  protected def runningQuery[A](result: Result, stream: fs2.Stream[F, A]): fs2.Stream[F, A] = stream

  protected def readRecord[A](record: Record, reader: Reader[A]): F[A] = ce.catchNonFatal(reader(record))
}

object Neo4jCypherTransactor {
  type Tx[F[_], R] = CypherTransactor.Tx[F, Record, fs2.Stream[F, *], R]

  def apply[F[_]: ConcurrentEffect: ContextShift](driver: Driver): Neo4jCypherTransactor[F] =
    new Neo4jCypherTransactor(Sync[F].delay{ driver.session() })

  def imapK[F[_], G[_]: Monad](f: F ~> G, g: G ~> F): Tx[F, *] ~> Tx[G, *] =
    λ[Tx[F, *] ~> Tx[G, *]](_
      .mapK(f)
      .compile(
        λ[Operation[Record, fs2.Stream[F, *], *] ~> Operation[Record, fs2.Stream[G, *], *]](_
          .imapK(
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
        def name: String = nme
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

    implicit def productReader[T <: Product, Repr <: HList](
      implicit gen: Generic.Aux[T, Repr],
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
          val names: List[String] = Nil
          def apply(src: Values): (Values, HNil) = (src, HNil)
        }

      implicit def hconsReader[H, T <: HList](
        implicit headReader: RootReader[H],
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
      knownValues.get(v.`type`())
                 .map(_(v).asInstanceOf[AnyRef])
                 .getOrElse(v.asObject())

    protected val knownValues: Map[Type, Reader[Value, _]]
  }

  trait DefaultValueReaders extends AnyValueReader {
    protected val Type = InternalTypeSystem.TYPE_SYSTEM

    protected val knownValues: Map[Type, Reader[Value, _]] = Map(
      Type.STRING()  -> defaultNeo4jStringReader,
      Type.INTEGER() -> defaultNeo4jLongReader,
      Type.FLOAT()   -> defaultNeo4jDoubleReader,
      Type.BOOLEAN() -> defaultNeo4jBooleanReader,
      Type.BYTES()   -> defaultNeo4jBytesReader,
      Type.NULL()    -> ValueReader("null", _ => None), // `null` wouldn't cause class cast exception
      Type.LIST()    -> defaultNeo4jListReader(defaultNeo4jAnyReader),
      Type.MAP()     -> defaultNeo4jMapReader(defaultNeo4jAnyReader),
      Type.NODE()         -> defaultNeo4jNodeReader,
      Type.RELATIONSHIP() -> defaultNeo4jRelReader,
      Type.PATH()         -> defaultNeo4jPathReader
    )

    implicit lazy val defaultNeo4jStringReader : ValueReader[String]      = ValueReader("String",      _.asString())
    implicit lazy val defaultNeo4jIntReader    : ValueReader[Int]         = ValueReader("Int",         _.asInt())
    implicit lazy val defaultNeo4jLongReader   : ValueReader[Long]        = ValueReader("Long",        _.asLong())
    implicit lazy val defaultNeo4jFloatReader  : ValueReader[Float]       = ValueReader("Float",       _.asFloat())
    implicit lazy val defaultNeo4jDoubleReader : ValueReader[Double]      = ValueReader("Double",      _.asDouble())
    implicit lazy val defaultNeo4jBooleanReader: ValueReader[Boolean]     = ValueReader("Boolean",     _.asBoolean())
    implicit lazy val defaultNeo4jBytesReader  : ValueReader[Array[Byte]] = ValueReader("Array[Byte]", _.asByteArray())
    implicit lazy val defaultNeo4jAnyRefReader : ValueReader[AnyRef]      = ValueReader("AnyRef",      readValue)
    implicit lazy val defaultNeo4jAnyReader    : ValueReader[Any]         = ValueReader("Any",         readValue)
    implicit lazy val defaultNeo4jBigDecReader : ValueReader[BigDecimal]  = ValueReader("BigDecimal",  BigDecimal apply _.asString())
    implicit lazy val defaultNeo4jBigIntReader : ValueReader[BigInt]      = ValueReader("BigInt",      BigInt     apply _.asString())

    implicit lazy val defaultNeo4jNodeReader: ValueReader[cypher.GraphElem.Node] = ValueReader("Node", v => mkNode(v.asNode()))
    implicit lazy val defaultNeo4jRelReader : ValueReader[cypher.GraphElem.Rel]  = ValueReader("Rel",  v => mkRel (v.asRelationship()))
    implicit lazy val defaultNeo4jPathReader: ValueReader[cypher.GraphPath]      = ValueReader("Path", v => mkPath(v.asPath()))

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
      def sourceName: String = "Record"
      def name: String = r.name
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

  class Syntax[F[_]: Applicative](implicit compiler: fs2.Stream.Compiler[F, F])
    extends CypherTransactor.Syntax[F, Record, fs2.Stream[F, *]]
  {
    syntax =>

    type TxC[A]       = CypherTransactor.TxC[F, Record, fs2.Stream[F, *], A]
    type TxG[G[_], A] = CypherTransactor.Tx [F, Record, fs2.Stream[F, *], G[A]]

    def gatherStream[G[_]](to: fs2.Stream.CompileOps[F, F, *] ~> λ[A => F[G[A]]]): Tx ~> λ[A => Tx[G[A]]] =
      gather.andThen[TxG[G, *]](λ[TxC ~> TxG[G, *]](_.flatMap(s => liftF(to(s.compile)))))

    override val ops: Neo4jOps = new Neo4jOps
    protected class Neo4jOps extends Ops {

      final implicit class SyntaxGatherStreamOps[A](tx: Tx[A]) {
        def gatherStream[C[_]](to: fs2.Stream.CompileOps[F, F, A] => F[C[A]]): Tx[C[A]] = {
          def func[X](ops: fs2.Stream.CompileOps[F, F, X]): F[C[X]] = to.asInstanceOf[fs2.Stream.CompileOps[F, F, X] => F[C[X]]](ops)
          val funcK = FunctionK.lift[fs2.Stream.CompileOps[F, F, *], λ[A => F[C[A]]]](func)
          syntax.gatherStream(funcK)(tx)
        }
      }
    }

    final protected def drainC[R]: CypherTransactor.TxC[F, Record, fs2.Stream[F, *], R] => Tx[Unit] =
      _.flatMap(s => liftF(s.compile.drain))
  }

}