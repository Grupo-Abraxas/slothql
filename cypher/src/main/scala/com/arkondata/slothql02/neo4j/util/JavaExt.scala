package com.arkondata.slothql02.neo4j.util

import scala.language.implicitConversions

import org.neo4j.driver.v1.util.{ Function => NFunction }


object JavaExt {
  implicit def neo4jFunction[A, B](f: A => B): NFunction[A, B] =
    new NFunction[A, B] {
      def apply(t: A): B = f(t)
    }
}
