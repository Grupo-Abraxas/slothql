package com.arkondata.slothql.neo4j

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import cats.effect.{ Resource, Sync }
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{ ~>, Eval, Id }
import io.opentracing.{ Span, SpanContext, Tracer }

import com.arkondata.opentracing.Tracing.TracingSetup
import com.arkondata.opentracing.effect.{ activeSpan, ResourceTracingOps }
import com.arkondata.opentracing.fs2.{ fs2StreamTracing, logStreamElems }
import com.arkondata.opentracing.util.TraceBundle
import com.arkondata.opentracing.{ SpanLog, TraceLaterEval, Tracing }
import com.arkondata.slothql.neo4j.Neo4jCypherTransactor

@compileTimeOnly(
  "Macro transformation was not applied. " +
  "Have you forgotten to set `-Ymacro-annotations` compiler flag (Scala 2.13) " +
  "or add `org.scalamacros:paradise` plugin (Scala 2.12)?"
)
class TransactorTracing extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransactorTracingMacros.transform
}

object TransactorTracing {
  type Endo[A]     = A => A
  type EndoK[F[_]] = F ~> F

  @compileTimeOnly("@TransactorTracing macro annotation must be put on the enclosing class")
  def setup[F[_]](
    runRead: EndoK[fs2.Stream[F, *]] = null,
    runWrite: EndoK[fs2.Stream[F, *]] = null,
    apply: EndoK[fs2.Stream[F, *]] = null,
    // transaction folding //
    runUnwind: EndoK[fs2.Stream[F, *]] = null,
    runGather: EndoK[λ[A => fs2.Stream[F, fs2.Stream[F, A]]]] = null,
    runQuery: EndoK[fs2.Stream[F, *]] = null
  ): Nothing = ???

  type EagerTracingInterface = Tracing.Interface[TraceLaterEval.Builder[cats.Id]]
  type EagerTracing          = EagerTracingInterface => TraceLaterEval.Builder[cats.Id]

  class TracingHelper[F[_]](implicit sync: Sync[F], traceBundle: TraceBundle.Endo[F]) {
    import traceBundle.Implicits._

    private val streamTracing = fs2StreamTracing[F]

    def traceStreamK: Tracing.Interface[EndoK[fs2.Stream[F, *]]] = new streamTracing.InterfaceImpl(locally)

    def logStreamK(log: (SpanLog, Any) => Unit): EndoK[fs2.Stream[F, *]] =
      λ[fs2.Stream[F, *] ~> fs2.Stream[F, *]](logStreamElem1(_, log))

    private def logStreamElem1[A](s: fs2.Stream[F, A], log: (SpanLog, Any) => Unit): fs2.Stream[F, A] =
      logStreamElems(s)(log.asInstanceOf[(SpanLog, A) => Unit])

    def traceResource[A](f: ResourceTracingOps[F, A] => Resource[F, A]): Endo[Resource[F, A]] = r =>
      f(new ResourceTracingOps(r))

    def traceLog(f: SpanLog => Unit): F[Unit] =
      activeSpan.flatMap(_.traverse_(s => sync.delay(f(s))))

    def trace(f: (Option[Span], Tracing.Interface[EndoK[F]]) => Any => EndoK[F]): EndoK[F] = λ[F ~> F](fa =>
      for {
        span <- activeSpan[F]
        i = new traceBundle.tracing.InterfaceImpl(locally)
        a <- fa
        endo = f(span, i)(a)
        res <- endo(sync.pure(a))
      } yield res
    )
  }

  object TracingHelper {
    def apply[F[_]: Sync: TraceBundle.Endo]: TracingHelper[F] = new TracingHelper
  }

  trait LostSpanFixes[F[_]] extends Neo4jCypherTransactor[F] {
    protected def lostSpanFixesTracer: Tracer
    protected def lostSpanFixesTracingSetup: TracingSetup

    implicit private def tracer       = lostSpanFixesTracer
    implicit private def tracingSetup = lostSpanFixesTracingSetup

    protected def runInsideTxWork(span: Option[Span])(run: => Unit): Unit = run

    protected lazy val laterEvalTracing = Tracing.tracingEvalLater

    class EagerTracingInterfaceWithParent(parent: Option[Either[Span, SpanContext]])
        extends laterEvalTracing.InterfaceImpl[TraceLaterEval.Builder[cats.Id]](f =>
          new TraceLaterEval.Builder(f.andThen(λ[Eval ~> cats.Id](_.value)))
        ) {

      override def apply(
        parent0: Option[Either[Span, SpanContext]],
        activate: Boolean,
        operation: String,
        tags: Map[String, Tracing.TagValue]
      ): TraceLaterEval.Builder[Id] =
        super.apply(parent0 orElse parent, activate, operation, tags)
    }
  }

  val setupDeclarationName: String = "transactorTracingSetup"
}

class TransactorTracingMacros(val c: whitebox.Context) {
  import c.universe._

  import TransactorTracing.setupDeclarationName

  def transform(annottees: Tree*): Tree = {
    val (classDef0, rest) = annottees.partition {
      case ClassDef(_, _, _, _) => true
      case _                    => false
    }
    val classDef = classDef0.headOption
      .getOrElse(c.abort(c.enclosingPosition, Msg.notClass))
      .asInstanceOf[ClassDef]
    val (setup0, newBody0) = findSetup(classDef)
    val setup              = setup0.getOrElse(c.abort(classDef.pos, Msg.noSetup))

    val F = classDef.impl.parents.collectFirst { case q"${tq"$_[$tpe]"}(..$_)" => tpe }
      .getOrElse(c.abort(c.enclosingPosition, Msg.notFoundTypeF))

    val (extraParents, extraBody) = setup.map { case (name, t) =>
      None -> overrideMethod(TransactorType.decl(TermName(name)).asMethod, t, unitMethods contains name)
    }.unzip

    val newParents = classDef.impl.parents ::: lostSpanFixesParent(F) :: extraParents.flatten

    val newBody = newBody0 ::: bodyHeader ::: lostSpanFixesBody(F) ::: extraBody

    val newClassTemplate = Template(newParents, classDef.impl.self, newBody)
    val newClassDef      = ClassDef(classDef.mods, classDef.name, classDef.tparams, newClassTemplate)

    c.info(c.enclosingPosition, showCode(newClassDef), force = true)

    q"..${newClassDef +: rest}"
  }

  val unitMethods = Set("inRunningTransactionWork", "commitTransaction", "rollbackTransaction", "closeTransaction")

  protected def bodyHeader: List[Tree] = List(
    q"import _root_.scala.concurrent.duration.FiniteDuration",
    q"import _root_.cats.effect._",
    q"import _root_.cats.syntax.apply._",
    q"import _root_.fs2.Stream",
    q"import _root_.org.neo4j.driver._",
    q"import _root_.com.arkondata.slothql.cypher.CypherTransactor.Operation",
    q"import _root_.com.arkondata.slothql.cypher.CypherStatement.Prepared"
  )

  private object Msg {
    def notClass = "@TransactorTracing macro should only be put on classes."

    def noSetup =
      s"Please setup tracing configuration by defining `val $setupDeclarationName` = TransactorTracing.setup(...)"

    def unexpectedSetup =
      "Unexpected tree. Expecting direct call to `com.arkondata.slothql.neo4j.TransactorTracing.setup` function."
    def notFoundTypeF = "Failed to determine effect type."
  }

  protected def findSetup(clazz: ClassDef): (Option[List[(String, Tree)]], List[Tree]) = {
    val (setup0, rest0) = clazz.impl.body.map {
      case s: ValOrDefDef if s.name == TermName(setupDeclarationName) =>
        s.rhs match {
          case q"$_[$_](..$params)" /*if setup.symbol == setupSymbol*/ =>
            val ps = params.map {
              case NamedArg(Ident(TermName(name)), t) => name -> t
              case t                                  => c.abort(t.pos, "Expecting named argument.")
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

  private def mkArgs(s: List[Symbol])   = s.map(mkArg)
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

  protected def lostSpanFixesParent(F: Tree) =
    tq"_root_.com.arkondata.slothql.neo4j.TransactorTracing.LostSpanFixes[$F]"

  protected def lostSpanFixesBody(F: Tree) =
    q"""
      protected def lostSpanFixesTracer: _root_.io.opentracing.Tracer =
        implicitly[_root_.com.arkondata.opentracing.util.TraceBundle.Endo[$F]].tracer
    """ ::
    q"""
      protected def lostSpanFixesTracingSetup: _root_.com.arkondata.opentracing.Tracing.TracingSetup =
        implicitly[_root_.com.arkondata.opentracing.util.TraceBundle.Endo[$F]].setup
    """ :: Nil

  private lazy val TransactorType =
    rootMirror.staticClass("com.arkondata.slothql.neo4j.Neo4jCypherTransactor").typeSignature
}
