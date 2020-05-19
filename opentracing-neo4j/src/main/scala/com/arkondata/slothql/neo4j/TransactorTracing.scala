package com.arkondata.slothql.neo4j

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.jdk.FutureConverters._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import cats.effect.concurrent.MVar
import cats.effect.syntax.bracket._
import cats.effect.syntax.effect._
import cats.effect.{ Async, Blocker, Resource, Sync }
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{ Eval, Id, ~> }
import com.github.fehu.opentracing.Tracing.TracingSetup
import com.github.fehu.opentracing.{ SpanLog, TraceLaterEval, Tracing }
import com.github.fehu.opentracing.effect.{ ResourceTracingOps, activateSpan, activeSpan }
import com.github.fehu.opentracing.fs2.{ fs2StreamTracing, logStreamElems }
import com.github.fehu.opentracing.util.TraceBundle
import io.opentracing.{ Span, SpanContext, Tracer }
import org.neo4j.driver.async.ResultCursor
import org.neo4j.driver.summary.ResultSummary
import org.neo4j.driver.{ Result, Session, Transaction, TransactionWork }

@compileTimeOnly("Macro transformation was not applied. " +
                 "Have you forgotten to set `-Ymacro-annotations` compiler flag (Scala 2.13) " +
                                      "or add `org.scalamacros:paradise` plugin (Scala 2.12)?" )
class TransactorTracing extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransactorTracingMacros.transform
}

object TransactorTracing {
  type Endo[A]     = A => A
  type EndoK[F[_]] = F ~> F

  @compileTimeOnly("@TransactorTracing macro annotation must be put on the enclosing class")
  def setup[F[_]](
    runRead:  EndoK[fs2.Stream[F, *]] = null,
    runWrite: EndoK[fs2.Stream[F, *]] = null,
    run:      EndoK[fs2.Stream[F, *]] = null,
    // dependencies of `run` //
    blockerResource:     Endo[Resource[F, Blocker]]     = null,
    sessionResource:     Endo[Resource[F, Session]]     = null,
    transactionResource: Endo[Resource[F, Transaction]] = null,
    // dependencies of `transactionResource` //
    backgroundWorkResource: Endo[Resource[F, F[Unit]]] = null,
    runInsideTxWork:        EagerTracing               = null.asInstanceOf[EagerTracing],
    commitTransaction:      F[Unit]                    = null.asInstanceOf[F[Unit]],
    rollbackTransaction:    F[Unit]                    = null.asInstanceOf[F[Unit]],
    closeTransaction:       F[Unit]                    = null.asInstanceOf[F[Unit]],
    // transaction folding //
    runUnwind: EndoK[fs2.Stream[F, *]]                        = null,
    runGather: EndoK[λ[A => fs2.Stream[F, fs2.Stream[F, A]]]] = null,
    runQuery:  EndoK[fs2.Stream[F, *]]                        = null,
    // reading result //
    readRecord:    EndoK[F]                 = null,
    reportSummary: ResultSummary => F[Unit] = null
  ): Nothing = ???

  type EagerTracingInterface = Tracing.Interface[TraceLaterEval.Builder[cats.Id]]
  type EagerTracing = EagerTracingInterface => TraceLaterEval.Builder[cats.Id]

  class TracingHelper[F[_]](implicit sync: Sync[F], traceBundle: TraceBundle.Endo[F]) {
    import traceBundle.Implicits._

    private val streamTracing = fs2StreamTracing[F]

    def traceStreamK: Tracing.Interface[EndoK[fs2.Stream[F, *]]] = new streamTracing.InterfaceImpl(locally)

    def logStreamK(log: (SpanLog, Any) => Unit): EndoK[fs2.Stream[F, *]] =
      λ[fs2.Stream[F, *] ~> fs2.Stream[F, *]](logStreamElem1(_, log))

    private def logStreamElem1[A](s: fs2.Stream[F, A], log: (SpanLog, Any) => Unit): fs2.Stream[F, A] =
      logStreamElems(s)(log.asInstanceOf[(SpanLog, A) => Unit])

    def traceResource[A](f: ResourceTracingOps[F, A] => Resource[F, A]): Endo[Resource[F, A]] = r => f(new ResourceTracingOps(r))

    def traceLog(f: SpanLog => Unit): F[Unit] =
      activeSpan.flatMap(_.traverse_(s => sync.delay(f(s))))

    def trace(f: (Option[Span], Tracing.Interface[EndoK[F]]) => Any => EndoK[F]): EndoK[F] = λ[F ~> F](
      fa =>
        for {
          span <- activeSpan[F]
          i    =  new traceBundle.tracing.InterfaceImpl(locally)
          a    <- fa
          endo =  f(span, i)(a)
          res  <- endo(sync.pure(a))
        } yield res
    )
  }

  object TracingHelper {
    def apply[F[_]: Sync: TraceBundle.Endo]: TracingHelper[F] = new TracingHelper
  }

  trait LostSpanFixes[F[_]] extends Neo4jCypherTransactor[F] {
    protected def lostSpanFixesTracer: Tracer
    protected def lostSpanFixesTracingSetup: TracingSetup

    private implicit def tracer = lostSpanFixesTracer
    private implicit def tracingSetup = lostSpanFixesTracingSetup

    override protected def readTxAsResource(txVar: MVar[F, Transaction]): Resource[F, Transaction] =
      for {
        span <- Resource liftF activeSpan[F]
        tx   <- super.readTxAsResource(txVar)
        _    <- Resource liftF activateSpan(span)
      } yield tx

    protected def runInsideTxWork(span: Option[Span])(run: => Unit): Unit = run

    override protected def transactionResource(run: TransactionWork[Unit] => Unit): Resource[F, Transaction] =
      for {
        txVar <- transactionMVarResource
        lock  <- lockMVarResource
        span  <- Resource liftF activeSpan[F]
        runTx = ce.delay(run{ tx =>
                          runInsideTxWork(span) {
                            ( for {
                                _ <- txVar.put(tx)
                                span1 <- activeSpan[F]
                                b <- lock.read
                                _ <- activateSpan(span1) // `activeSpan` is lost otherwise
                                _ <- if (b) commitTransaction(tx) else rollbackTransaction(tx)
                              } yield ()
                            ).guarantee(closeTransaction(tx))
                             .toIO.unsafeRunSync()
                          }
                      })
        _     <- backgroundWorkResource(runTx)
        tx    <- readTxAsResource(txVar)
      } yield tx

    protected lazy val laterEvalTracing = Tracing.tracingEvalLater
    class EagerTracingInterfaceWithParent(parent: Option[Either[Span, SpanContext]])
      extends laterEvalTracing.InterfaceImpl[TraceLaterEval.Builder[cats.Id]](f =>
        new TraceLaterEval.Builder(f.andThen(λ[Eval ~> cats.Id](_.value)))
      ) {
      override def apply(parent0: Option[Either[Span, SpanContext]], activate: Boolean, operation: String, tags: Map[String, Tracing.TagValue]): TraceLaterEval.Builder[Id] =
        super.apply(parent0 orElse parent, activate, operation, tags)
    }
  }

  trait SummaryReport[F[_]] extends Neo4jCypherTransactor[F] {
    protected def reportSummary(summary: ResultSummary): F[Unit]

    override protected def runningQuery[A](result: Result, stream: fs2.Stream[F, A]): fs2.Stream[F, A] = {
      stream.onFinalize {
        for {
          cursor  <- internalResultCursor(result)
          summary <- Async.fromFuture(ce.delay{ cursor.consumeAsync().asScala })
          _       <- reportSummary(summary)
        } yield ()
      }
    }

    // Use java reflection for getting access to private field
    private def internalResultCursor(result: Result): F[ResultCursor] = for {
      clazz  <- ce.pure(result.getClass)
      field  <- ce.catchNonFatal{ clazz.getDeclaredField("cursor") }
      _      <- ce.catchNonFatal{ field.setAccessible(true) }
      cursor <- ce.catchNonFatal{ field.get(result).asInstanceOf[ResultCursor] }
    } yield cursor
  }

  val setupDeclarationName: String = "transactorTracingSetup"
}

class TransactorTracingMacros(val c: whitebox.Context) {
  import c.universe._

  import TransactorTracing.setupDeclarationName

  def transform(annottees: Tree*): Tree = {
    val (classDef0, rest) = annottees.partition{ case ClassDef(_, _, _, _) => true
                                                 case _ => false }
    val classDef = classDef0.headOption
                    .getOrElse(c.abort(c.enclosingPosition, Msg.notClass))
                    .asInstanceOf[ClassDef]
    val (setup0, newBody0) = findSetup(classDef)
    val setup = setup0.getOrElse(c.abort(classDef.pos, Msg.noSetup))

    val F = classDef.impl.parents
                    .collectFirst { case q"${tq"$_[$tpe]"}(..$_)" => tpe }
                    .getOrElse(c.abort(c.enclosingPosition, Msg.notFoundTypeF))

    val (extraParents, extraBody) = setup.map {
      case ("reportSummary", t)   => reportSummaryImpl(F, t)
      case ("run", t)             => None -> runImplTree(t)
      case ("runInsideTxWork", t) => None -> runningInsideTransactionWorkImpl(t)
      case (name, t) =>
        None -> overrideMethod(TransactorType.decl(TermName(name)).asMethod, t, unitMethods contains name)
    }.unzip

    val newParents = classDef.impl.parents ::: lostSpanFixesParent(F) :: extraParents.flatten

    val newBody = newBody0 ::: bodyHeader ::: lostSpanFixesBody(F) ::: extraBody

    val newClassTemplate = Template(newParents, classDef.impl.self, newBody)
    val newClassDef = ClassDef(classDef.mods, classDef.name, classDef.tparams, newClassTemplate)

    c.info(c.enclosingPosition, showCode(newClassDef), force = true)

    q"..${newClassDef +: rest}"
  }

  val unitMethods = Set("inRunningTransactionWork", "commitTransaction", "rollbackTransaction", "closeTransaction")

  protected def bodyHeader: List[Tree] = List(
    q"import _root_.cats.effect._",
    q"import _root_.cats.syntax.apply._",
    q"import _root_.fs2.Stream",
    q"import _root_.org.neo4j.driver._",
    q"import _root_.com.arkondata.slothql.cypher.CypherTransactor.Operation",
    q"import _root_.com.arkondata.slothql.cypher.CypherStatement.Prepared"
  )

  protected def runImplTree(wrap: Tree) =
    q"""
      override protected def run[A](
        tx: Tx[A],
        txWork0: _root_.org.neo4j.driver.Session => (_root_.org.neo4j.driver.TransactionWork[B] => B) forSome {type B}
      ): Out[A] = $wrap(super.run(tx, txWork0))
    """

  protected def runningInsideTransactionWorkImpl(t: Tree) =
    q"""
      override protected def runInsideTxWork(span: Option[_root_.io.opentracing.Span])(run: => _root_.scala.Unit): _root_.scala.Unit =
       $t(new this.EagerTracingInterfaceWithParent(span.map(_root_.scala.Left(_))))(run)
    """

  protected def reportSummaryImpl(F: Tree, wrap: Tree) = {
    val parent = tq"_root_.com.arkondata.slothql.neo4j.TransactorTracing.SummaryReport[$F]"
    val impl = q"protected def reportSummary(summary: _root_.org.neo4j.driver.summary.ResultSummary): $F[_root_.scala.Unit] = $wrap(summary)"
    Some(parent) -> impl
  }

  private object Msg {
    def notClass = "@TransactorTracing macro should only be put on classes."
    def noSetup = s"Please setup tracing configuration by defining `val $setupDeclarationName` = TransactorTracing.setup(...)"
    def unexpectedSetup = "Unexpected tree. Expecting direct call to `com.arkondata.slothql.neo4j.TransactorTracing.setup` function."
    def notFoundTypeF = "Failed to determine effect type."
  }

  protected def findSetup(clazz: ClassDef): (Option[List[(String, Tree)]], List[Tree]) = {
    val (setup0, rest0) = clazz .impl.body.map {
      case s: ValOrDefDef if s.name == TermName(setupDeclarationName) =>
        s.rhs match {
          case q"$_[$_](..$params)" /*if setup.symbol == setupSymbol*/ =>
            val ps = params.map { case NamedArg(Ident(TermName(name)), t) => name -> t
                                  case t => c.abort(t.pos, "Expecting named argument.")
                                }
            (Some(ps), None)
          case _ => c.abort(s.rhs.pos, Msg.unexpectedSetup)
        }
      case other => (None, Some(other))
    }.unzip
    (setup0.flatten.headOption, rest0.flatten)
  }

  protected def overrideMethod(s: MethodSymbol, f: Tree, compose: Boolean) = {
    val flags = Option.when(s.isProtected)(Flag.PROTECTED).toList :::
                Option.when(s.isStable)(Flag.STABLE).toList
    val mods = Modifiers(flags.fold(Flag.OVERRIDE)(_ | _))
    val name = s.name
    val retT = detachType(s.returnType)
    val wrap = if (compose) q"(_: $retT) <* $f" else f
    (s.typeParams, s.paramLists) match {
      case (Nil, Nil) =>
        q"$mods def $name: $retT = $wrap(super.$name)"
      case (Nil, List(ps)) =>
        q"$mods def $name(..${ps.map(mkParam)}): $retT = $wrap(super.$name(..${ps.map(mkArg)}))"
      case (List(t1), List(ps)) =>
        val t1n = t1.name.toTypeName
        q"$mods def $name[$t1n](..${mkParams(ps)}): $retT = $wrap(super.$name[$t1n](..${mkArgs(ps)}))"
      case (List(t1), List(ps1, ps2)) =>
        val t1n = t1.name.toTypeName
        q"""
          $mods def $name[$t1n](..${mkParams(ps1)})(..${mkParams(ps2)}): $retT =
            $wrap(super.$name[$t1n](..${mkArgs(ps1)})(..${mkArgs(ps2)}))
        """
      case (List(t1, t2), List(ps1, ps2)) =>
        val t1n = t1.name.toTypeName
        val t2n = t2.name.toTypeName
        q"""
          $mods def $name[$t1n, $t2n](..${mkParams(ps1)})(..${mkParams(ps2)}): $retT =
            $wrap(super.$name[$t1n, $t2n](..${mkArgs(ps1)})(..${mkArgs(ps2)}))
        """
    }
  }

  private def mkArgs(s: List[Symbol]) = s.map(mkArg)
  private def mkParams(s: List[Symbol]) = s.map(mkParam)

  private def mkArg(s: Symbol) = q"${s.name.toTermName}"
  private def mkParam(s: Symbol) =
    ValDef(Modifiers(Flag.PARAM), s.name.toTermName, detachType(s.typeSignature.resultType), EmptyTree)

  private def detachType(tpe: Type): Tree = tpe match {
    case TypeRef(_, t, Nil)  => tq"${t.name.toTypeName}"
    case TypeRef(_, t, args) => tq"${t.name.toTypeName}[..${args.map(detachType)}]"
    case ExistentialType(es, TypeRef(_, t, ps)) =>
      val ps1 = ps.map {
        case tpe if es contains tpe.typeSymbol => typeNames.WILDCARD
        case tpe                               => tpe.typeSymbol.name.toTypeName
      }
      tq"${t.name.toTypeName}[..$ps1]"
    case other =>
      c.info(c.enclosingPosition, s"other = ${showRaw(other)}", force = true)
      tq"${other.typeSymbol.name.toTypeName}"
  }

  protected def lostSpanFixesParent(F: Tree) = tq"_root_.com.arkondata.slothql.neo4j.TransactorTracing.LostSpanFixes[$F]"
  protected def lostSpanFixesBody(F: Tree) =
    q"""
      protected def lostSpanFixesTracer: _root_.io.opentracing.Tracer =
        implicitly[_root_.com.github.fehu.opentracing.util.TraceBundle.Endo[$F]].tracer
    """ ::
    q"""
      protected def lostSpanFixesTracingSetup: _root_.com.github.fehu.opentracing.Tracing.TracingSetup =
        implicitly[_root_.com.github.fehu.opentracing.util.TraceBundle.Endo[$F]].setup
    """ :: Nil

  private lazy val TransactorType = rootMirror.staticClass("com.arkondata.slothql.neo4j.Neo4jCypherTransactor").typeSignature
}
