package com.arkondata.slothql

import cats.effect.{ IO, Resource }
import natchez.Span
import natchez.jaeger.Jaeger

trait TracingSpec {

  lazy val entryPointApp: Resource[IO, Span[IO]] = Jaeger
    .entryPoint[IO]("system-1") { cfg =>
      IO(cfg.withServiceName("service-1").getTracer)
    }
    .flatMap(_.root("root-span"))
    .flatMap(_.span("inner-span"))
}
