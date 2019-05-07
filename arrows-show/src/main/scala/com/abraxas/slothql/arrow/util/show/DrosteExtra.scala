package com.abraxas.slothql.arrow.util.show

import scala.language.higherKinds

import qq.droste.Scatter
import qq.droste.data.Coattr

object DrosteExtra {
  object Scatter {
    def futu[F[_], A]: Scatter[F, A, Coattr[F, A]] = {
      case Coattr.Pure(e) => Left(e)
      case Coattr.Roll(d) => Right(d)
    }
  }
}
