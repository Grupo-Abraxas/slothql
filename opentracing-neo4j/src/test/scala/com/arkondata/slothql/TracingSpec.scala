package com.arkondata.slothql

import cats.effect.{ IO, Resource }
import io.jaegertracing.Configuration.{ ReporterConfiguration, SamplerConfiguration }
import natchez.Span
import natchez.jaeger.Jaeger

trait TracingSpec {

  import io.jaegertracing.Configuration

  System.setProperty(Configuration.JAEGER_SERVICE_NAME, "tracing-spec-?")
  System.setProperty(Configuration.JAEGER_AGENT_HOST, "jaegertracing")
  System.setProperty(Configuration.JAEGER_SAMPLER_TYPE, "const")
  System.setProperty(Configuration.JAEGER_SAMPLER_PARAM, "1")

  lazy val entryPointApp: Resource[IO, Span[IO]] = Jaeger
    .entryPoint[IO]("system-1") { cfg =>
      IO(
        cfg
          .withSampler(SamplerConfiguration.fromEnv)
          .withReporter(ReporterConfiguration.fromEnv)
          .getTracer
      )
    }
    .flatMap(_.root("root-span"))
    .flatMap(_.span("inner-span"))
}
