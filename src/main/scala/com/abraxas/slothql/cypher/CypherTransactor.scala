package com.abraxas.slothql.cypher

import scala.language.higherKinds

import cats.effect.IO
import shapeless.DepFn1

import com.abraxas.slothql.cypher.CypherFragment.{ Known, Query }


trait CypherTransactor {
  type Result
  type Reader[A] <: CypherTransactor.Reader[Result, A]
  def read[A](query: Known[Query[A]])(implicit read: Reader[A]): IO[Seq[read.Out]]
}

object CypherTransactor {
  type Aux[R, RR[x] <: Reader[R, x]] = CypherTransactor { type Result = R; type Reader[A] = RR[A] }

  trait Reader[Src, A] extends DepFn1[Src]
  object Reader {
    type Aux[Src, A, R] = Reader[Src, A] { type Out = R }
    def apply[Src, A](implicit reader: Reader[Src, A]): Aux[Src, A, reader.Out] = reader
  }
}
