package com.abraxas.slothql.util

object ClassUtils {
  object Show {
    def apply(c: Class[_]): String = apply(c, short = true)
    def apply(c: Class[_], short: Boolean): String = {
      val clazz = nonLocalClass(c)
      if (short) simpleName(clazz) else clazz.getName
    }

    private def nonLocalClass(c: Class[_]): Class[_] = if (c.isLocalClass) nonLocalClass(c.getSuperclass) else c
    private def simpleName(clazz: Class[_]): String = clazz.getName.split(Array('.', '$')).last
  }
}
