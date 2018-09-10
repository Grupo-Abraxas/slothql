package com.abraxas.slothql.util

import scala.runtime.ScalaRunTime.arrayElementClass

object ShowManifest {
  def apply(c: Class[_]): String = prettyprint(c, Nil, short = true)
  def apply(c: Class[_], short: Boolean): String = prettyprint(c, Nil, short)

  def apply(m: Manifest[_]): String = prettyprint(m.runtimeClass, m.typeArguments, short = true)
  def apply(m: Manifest[_], short: Boolean): String = prettyprint(m.runtimeClass, m.typeArguments, short)

  def noTypeParams(m: Manifest[_], short: Boolean = true): String = prettyprint(m.runtimeClass, Nil, short = short)

  private def prettyprint(clazz0: Class[_], tparams: List[Manifest[_]], short: Boolean): String = {
    val clazz = nonLocalClass(clazz0)
    if (clazz.isArray) s"Array[${prettyprint(arrayElementClass(clazz), Nil, short)}]"
    else {
      val targs = if (tparams.nonEmpty) tparams.map(apply(_, short)).mkString("[", ", ", "]") else ""
      (if (short) simpleName(clazz) else clazz.getName) + targs
    }
  }

  private def nonLocalClass(c: Class[_]): Class[_] = if (c.isLocalClass) nonLocalClass(c.getSuperclass) else c
  private def simpleName(clazz: Class[_]): String = clazz.getName.split(Array('.', '$')).last
}
