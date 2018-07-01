package com.abraxas.slothql.util

import scala.runtime.ScalaRunTime.arrayElementClass

object ShowManifest {
  def apply(m: Manifest[_]): String = prettyprint(m.runtimeClass, m.typeArguments)

  private def prettyprint(clazz: Class[_], tparams: List[Manifest[_]]): String =
    if (clazz.isArray) s"Array[${prettyprint(arrayElementClass(clazz), Nil)}]"
    else {
      val targs = if (tparams.nonEmpty) tparams.map(apply).mkString("[", ", ", "]") else ""
      clazz.getSimpleName + targs
    }
}
