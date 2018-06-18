package com.abraxas.slothql.util

import shapeless.Witness

object Bool {
  type True  = Witness.`true`.T
  type False = Witness.`false`.T
}
