package com.abraxas.slothql.util

import scala.reflect.api.{ Mirror, TypeCreator, Universe }
import scala.reflect.runtime.{ universe => ru }

object TypeUtils {
  private lazy val ruMirror = ru.runtimeMirror(getClass.getClassLoader)

  def tagType[T](tpe: ru.Type): ru.TypeTag[T] = ru.TypeTag[T](ruMirror, new TypeCreator {
    // From [[https://stackoverflow.com/questions/27887386/get-a-typetag-from-a-type]]
    def apply[U <: Universe with Singleton](m: Mirror[U]): U#Type =
      if (m eq ruMirror) tpe.dealias.asInstanceOf[U#Type]
      else throw new IllegalArgumentException(s"Type tag defined in $ruMirror cannot be migrated to other mirrors.")
  })


  def tagTypeRef[T](tag: ru.TypeTag[_], args: List[ru.TypeTag[_]]): ru.TypeTag[T] = tagTypeRef(tag.tpe, args.map(_.tpe))
  def tagTypeRef[T](tpe: ru.Type, args: List[ru.Type]): ru.TypeTag[T] = tagType(mkTypeRef(tpe, args))


  def mkTypeRef(tpe: ru.Type, args: List[ru.Type]): ru.Type = {
    val ru.TypeRef(pre, sym, _) = tpe.typeConstructor
    ru.internal.typeRef(pre, sym, args)
  }


  object Show {
    def apply(tag: ru.TypeTag[_]): String = apply(tag.tpe)
    def apply(tpe0: ru.Type): String = {
      val tpe = tpe0.dealias
      val args = if (tpe.typeArgs.nonEmpty) tpe.typeArgs.map(Show(_)).mkString("[", ", ", "]") else ""
      tpe.typeSymbol.name.toString + args
    }

    def noTypeParams(tpe: ru.Type): String = tpe.dealias.typeSymbol.name.toString
  }
}
