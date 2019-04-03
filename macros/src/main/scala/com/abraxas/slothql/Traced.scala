package com.abraxas.slothql

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import com.github.fehu.opentracing.Tracing.TracingSetup
import com.github.fehu.opentracing.trace
import io.jaegertracing.Configuration
import io.jaegertracing.Configuration.SamplerConfiguration
import io.jaegertracing.internal.samplers.ConstSampler
import io.opentracing.Tracer
import io.opentracing.util.GlobalTracer
import shapeless.LazyMacros

trait Traced[+A] extends Serializable {
  val value: A
}

object Traced {
  def apply[A](a: A): Traced[A] = new Traced[A] { val value: A = a }

  implicit def tracedImplicit[A]: Traced[A] = macro TracedMacros.tracedImpl[A]

  class TracedMacros(c0: whitebox.Context) extends LazyMacros(c0) {
    import c.universe._

    def tracedImpl[A: WeakTypeTag]: c.Tree =
      trace.now("implicit",
                tags = "file" -> c.enclosingPosition.source.path,
                       "line" -> c.enclosingPosition.source.lineToString(c.enclosingPosition.line - 1),
                       "pos"  -> c.enclosingPosition.toString
      ){
        mkImpl[A](
          (tree, actualType) => {
            tracer.activeSpan().setTag("type", show(actualType))
            tracer.activeSpan().setOperationName(showShort(actualType))
            q"""
              new _root_.com.abraxas.slothql.Traced[$actualType]{
                val value: $actualType {} = $tree
              }
             """
          },
          q"null.asInstanceOf[_root_.com.abraxas.slothql.Traced[_root_.scala.Nothing]]"
        )
      }

    private def showName(name0: String): String =
      name0.takeWhile(_ != '{').split('.').reverse match {
        case Array("Aux", name, _*) => name
        case Array(name, _*) => name
      }
    private def showShort(tpe: Type): String = showName(tpe.typeConstructor.toString)
  }

  private implicit def tracingSetup: TracingSetup = TracingSetup.Dummy.DummyTracingSetup
  private implicit def tracer: Tracer = Jaeger.get()

  private object Jaeger {
    def get(): Tracer =
      if (GlobalTracer.isRegistered) GlobalTracer.get()
      else {
        val tracer = create()
        GlobalTracer.register(tracer)
        tracer
      }

    private def create(): Tracer = Configuration
      .fromEnv("compile")
      .withSampler(
        SamplerConfiguration.fromEnv()
          .withType(ConstSampler.TYPE)
          .withParam(1)
      )
      .getTracer
  }
}