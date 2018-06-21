package com.abraxas.slothql

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import shapeless.{ HList, Poly1 }

object FragmentDefinitionHelper {
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  class fragment[T] extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.apply
  }

  sealed trait NarrowMark[T]
  type fromNarrow[A, T <: A] = A with NarrowMark[T]

  sealed trait FromHList[L <: HList, F <: Poly1, Result <: Traversable[_]]
  sealed trait As[A, B]



  class Impl(val c: whitebox.Context) {
    import c.universe._

    def apply(annottees: c.Expr[Any]*): c.Expr[Any] = {
      val frag = Fragment(annottees)

      c.info(NoPosition,
        s"""
           |${frag.Trait.tree}
           |
           |${frag.Companion.tree}
         """.stripMargin,
        force = true
      )
      c.Expr[Any](
        q"""
          ${frag.Trait.tree}
          ${frag.Companion.tree}
         """
      )
    }

    private def undefined = q"_root_.scala.Predef.???"

    def typeCheckTpt(tpt: Tree): Type = c.typecheck(q"$undefined : $tpt").tpe

    private trait Definition
    private class AbstractType(val definition: TypeDef) extends Definition {
      @inline def name = definition.name
    }
    private class Field(val definition: ValDef) extends Definition {
      @inline def name = definition.name
      @inline def tpt = definition.tpt

      lazy val typeRelated: Option[TypeRelatedDirective] = directive.collect { case t: TypeRelatedDirective => t }

      def resultType: Tree = typeRelated.map(trd => TypeTree(trd.resultType)).getOrElse(tpt)

      val directive = PartialFunction.condOpt(definition) {
        case ValDef(_, _, AppliedTypeTree(Ident(TypeName("fromNarrow")), List(t0, Ident(tname: TypeName))), EmptyTree) =>
          NarrowType(tname, typeCheckTpt(t0))
        case ValDef(Modifiers(Flag.IMPLICIT | _), _, _, EmptyTree) =>
          ImplicitParam
      }

    }
    private class Other(val definition: ValOrDefDef) extends Definition   // TODO: not preserved yet

    private trait Directive
    private trait TypeRelatedDirective extends Directive {
      val usedTypeName: TypeName
      val resultType: Type
    }
    private case class NarrowType(usedTypeName: TypeName, resultType: Type) extends TypeRelatedDirective
    private case object ImplicitParam extends Directive

    private def parseDefinitions(defs: List[Tree]): Seq[Definition] =
      defs map {
        case t: TypeDef                       => new AbstractType(t)
        case v: ValOrDefDef if v.rhs.nonEmpty => new Other(v)
        case v: ValDef                        => new Field(v)
        case x => c.abort(x.pos, s"unsupported definition:\n$x")
      }


    private object Fragment {
      def apply(annottees: Seq[c.Expr[Any]]) = new Fragment(
        traitDef = annottees.headOption match {
          case Some(Expr(t: ClassDef)) if t.mods.hasFlag(Flag.TRAIT) => t
          case _ => c.abort(c.enclosingPosition, "Only traits can be annotated with @fragment.")
        },
        companionDef = annottees.tail.headOption.collect{ case Expr(m: ModuleDef) => m }
      )

    }

    private class Fragment(val traitDef: ClassDef, val companionDef: Option[ModuleDef]) {
      @inline def mods = traitDef.mods
      @inline def name = traitDef.name
      @inline def tparams = traitDef.tparams
      @inline def parents = traitDef.impl.parents
      @inline def self = traitDef.impl.self
      @inline def body0 = traitDef.impl.body

      lazy val tArgNames = tparams.map(_.name)

      private val definitions0 = parseDefinitions(body0)
      val abstractTypes = definitions0.collect{ case t: AbstractType => t.name -> t.definition }.toMap
      private val defFields0 = definitions0.collect{ case f: Field => f }
      val typeRelatedFields = defFields0.filter(_.typeRelated.isDefined)
      val implicitFields = defFields0.filter(_.directive.contains(ImplicitParam))

      object Trait {
        val body =
          abstractTypes.values ++
          abstractTypes.keys.map{ t => q"val ${t.toTermName}: $t" } ++
          typeRelatedFields.map{ f => q"val ${f.name}: ${f.resultType}"} ++
          implicitFields.map(_.definition)
        def tree = ClassDef(mods, name, tparams, Template(parents, self, body.toList))
      }

      object Companion {
        private def typeSuffix(td: TypeDef, suffix: String): TypeDef = td match {
          case TypeDef(_, name, tps, tpt) => TypeDef(Modifiers(Flag.PARAM), nameSuffix(name, suffix), tps, tpt)
        }
        private val tparams0 = tparams.map(typeSuffix(_, "0"))
        private val abstractTypes0 = abstractTypes.values.map(typeSuffix(_, "0"))

        def Aux: Tree =
          q"""
            type Aux[..${tparams ++ abstractTypes0}] =
              $name[..$tArgNames] {
                ..${
                  abstractTypes.values.zip(abstractTypes0).map {
                    case (t0, t1) => q"type ${t0.name} = ${t1.name}"
                  }
                }
              }
           """
        protected def implicits =
          if (implicitFields.isEmpty) q"dummy: _root_.scala.Predef.DummyImplicit" :: Nil
          else implicitFields.map(_.definition)

        // TODO: remove covariance/contravariance modifiers if any
        def apply: Tree =
          q"""
            def apply[..$tparams](..${typeRelatedFields.map{ f => q"${f.name}: ${f.resultType}" }})
                                 (implicit ..$implicits): $name[..$tArgNames] =
            {
              ..${(typeRelatedFields ++ implicitFields).map{ f =>
                  q"@_root_.scala.inline def ${nameSuffix(f.name, "0")} = ${f.name}"
                }}
              new $name[..$tArgNames] {
                ..${abstractTypes.values.map{ t => q"type ${t.name} = _root_.scala.Nothing" }}
                ..${abstractTypes.keys.map(   t => q"lazy val ${t.toTermName}: $t = $undefined")}
                ..${typeRelatedFields.map{    f => q"val ${f.name}: ${f.resultType} = ${nameSuffix(f.name, "0")}" }}
                ..${implicitFields.map{       f => q"implicit val ${f.name}: ${f.tpt} = ${nameSuffix(f.name, "0")}" }}
              }
             }
           """

        // TODO: remove covariance/contravariance modifiers if any
        def unapply: Tree =
          q"""
            def unapply[..$tparams](arg: ${parents.head}): _root_.scala.Option[(..${typeRelatedFields.map(_.resultType)})] =
              _root_.scala.PartialFunction.condOpt(arg) {
                case x: ${ tq"$name[..$tArgNames]" } @unchecked => (..${typeRelatedFields.map{ f => q"x.${f.name}" }})
              }
           """

        object TypedBuilder {
          private val typeNames: Set[TypeName] = tparams.map(_.name).toSet //  ++ abstractTypes.keySet

          object TypeNamesTransformer extends Transformer {
            override def transform(tree0: c.universe.Tree): c.universe.Tree = {
              val tree = tree0 match {
                case Ident(name: TypeName) if typeNames contains name => Ident(nameSuffix(name, "0"))
                case other => other
              }
              super.transform(tree)
            }
          }

          def implicitFields0: Seq[ValDef] = implicitFields.map{ f =>
            f.definition match {
              case ValDef(mods, name, tpt, EmptyTree) =>
                val newTpt = TypeNamesTransformer.transform(tpt)
                ValDef(mods, name, newTpt, EmptyTree) // TODO: mods?
            }
          }
          def implicits =
            if (implicitFields.isEmpty) q"dummy: _root_.scala.Predef.DummyImplicit" :: Nil
            else implicitFields0

          // TODO: should covariance/contravariance be preserved?
          def tree: Tree =
            q"""
              protected class TypedBuilder[..${tparams0 ++ abstractTypes0}](
                ..${typeRelatedFields.flatMap{ f =>
                    val name0 = nameSuffix(f.name, "0")
                    List(
                      q"val $name0: _root_.scala.Option[${f.resultType}] = _root_.scala.None",
                      q"val ${nameCapitalize(name0)}: _root_.scala.Option[${nameSuffix(f.typeRelated.get.usedTypeName, "0")}] = _root_.scala.None"
                    )
                  }}
              )(implicit ..$implicits) {

                ..${typeRelatedFields.flatMap{ f =>
                    f.typeRelated.get match {
                      case NarrowType(tName, resultType) =>
                        val t = abstractTypes(tName)
                        val methodName = namePrefix(f.name, "with")
                        // TODO: remove covariance/contravariance modifiers if any
                        q"""
                          def $methodName[$t](
                            ${f.name}: ${t.name}
                          )(implicit w: _root_.shapeless.Witness.Aux[${t.name}]) =
                            copy(
                              ${f.name} = _root_.scala.Option(${f.name}),
                              ${nameCapitalize(f.name)} = _root_.scala.Option(${f.name})
                            )
                         """ ::
                        // TODO: remove covariance/contravariance modifiers if any
                        q"""
                          private[$name] def ${nameSuffix(methodName, "0")}[$t](
                            ${nameCapitalize(f.name)}: ${t.name},
                            ${f.name}: $resultType
                          ) =
                            copy(
                              ${nameCapitalize(f.name)} = _root_.scala.Option(${nameCapitalize(f.name)}),
                              ${f.name} = _root_.scala.Option(${f.name})
                            )
                         """ :: Nil
                    }
                  }}

                def build(
                  implicit ..${abstractTypes.map { case (_, t) =>
                    q"""
                      val ${TermName("defined" + capitalize(t.name.toString))}
                        : _root_.shapeless.=:!=[${nameSuffix(nameCapitalize(t.name), "0")}, _root_.scala.Nothing]
                     """
                  }}
                ): ${name.toTermName}.Aux[..${(tArgNames ++ abstractTypes.keys).map(nameSuffix(_, "0"))}] =
                  new $name[..${tArgNames.map(nameSuffix(_, "0"))}] {
                    ..${abstractTypes.values.map{ t =>
                        q"type ${t.name} = ${nameSuffix(t.name, "0")}" }}
                    ..${abstractTypes.keys.map{ t =>
                        q"lazy val ${t.toTermName}: $t = ${nameSuffix(t.toTermName, "0")}.getOrElse($undefined)" }}
                    ..${typeRelatedFields.map{ f =>
                        q"val ${f.name}: ${f.resultType} = ${nameSuffix(f.name, "0")}.getOrElse($undefined)" }}
                    ..${implicitFields.map{ f =>
                        q"implicit val ${f.name}: ${TypeNamesTransformer.transform(f.tpt)} = TypedBuilder.this.${f.name}" }}
                  }

                private def copy[..${tparams ++ abstractTypes.values}](
                  ..${typeRelatedFields.flatMap{ f =>
                      val name0 = nameSuffix(f.name, "0")
                      List(
                        q"val ${f.name}: _root_.scala.Option[${f.resultType}] = $name0",
                        q"val ${nameCapitalize(f.name)}: _root_.scala.Option[${f.typeRelated.get.usedTypeName}] = ${nameCapitalize(name0)}"
                      )
                    }}
                )(implicit ..${Companion.implicits}) = new TypedBuilder[..${tArgNames ++ abstractTypes.keys}](
                  ..${typeRelatedFields.flatMap{ f => List(f.name, nameCapitalize(f.name)) }}
                )
              }
             """
        }

        // TODO: remove covariance/contravariance modifiers if any
        def typed: Tree = {
          val targs = tArgNames.map(Ident(_)) ++ abstractTypes.map(_ => tq"_root_.scala.Nothing")
          q"""
            def typed[..$tparams](implicit ..$implicits): TypedBuilder[..$targs] = new TypedBuilder[..$targs]
           """
        }

        def construct: Tree = {
          val Construct  =  q"_root_.com.abraxas.slothql.util.Construct"
          val ConstructT = tq"_root_.com.abraxas.slothql.util.Construct"
          val tps = tparams ++ abstractTypes.values
          val implicits = implicitFields.map(_.definition) ++ typeRelatedFields.map{ f =>
            f.typeRelated.get match {
              case NarrowType(tname, _) =>
                q"val ${nameSuffix(f.name, "W")}: _root_.shapeless.Witness.Aux[$tname]"
            }
          }
          val argsHList = abstractTypes.keys
            .foldLeft(tq"_root_.shapeless.HNil": Tree)((acc, t) => tq"_root_.shapeless.::[$t, $acc]" )
          val build0 = abstractTypes.keys.zipWithIndex
            .foldLeft(q"${name.toTermName}.typed[..$tArgNames]": Tree) {
              case (acc, (t, i)) =>
                q"$acc.${namePrefix(t, "with").toTermName}(args($i))"
            }

          q"""
            implicit def ${namePrefix(name, "construct").toTermName}[..$tps](
              implicit ..$implicits
            ): $Construct.Aux[$name[..$tArgNames], $argsHList, ${name.toTermName}.Aux[..${tArgNames ++ abstractTypes.keys}]] =
              new $ConstructT[$name[..$tArgNames], $argsHList] {
                type Out = ${name.toTermName}.Aux[..${tArgNames ++ abstractTypes.keys}]
                def apply(args: $argsHList): Out = $build0.build
              }
           """

        }

        def body = List(
          Aux, apply, unapply, TypedBuilder.tree, typed,
          construct
        )

        def tree = companionDef
          .map{
            case ModuleDef(mods, name, Template(parents, self, body0)) =>
              ModuleDef(mods, name, Template(parents, self, body ::: body0))
          }
          .getOrElse{
            ModuleDef(NoMods, name.toTermName, Template(Nil, noSelfType, body))
          }
      }
    }

    private def mapName[N <: Name](n: N, f: String => String): N = n match {
      case TermName(name) => TermName(f(name)).asInstanceOf[N]
      case TypeName(name) => TypeName(f(name)).asInstanceOf[N]
    }
    private def namePrefix[N <: Name](n: N, prefix: String, camelCase: Boolean = true): N = {
      val n0 = if (camelCase) nameCapitalize(n) else n
      mapName(n0, prefix + _)
    }
    private def nameSuffix[N <: Name](n: N, suffix: String): N = mapName(n, _ + suffix)
    private def nameCapitalize[N <: Name](n: N): N = mapName(n, capitalize)

    private def capitalize(s: String) = {
      val chars = s.toCharArray
      chars(0) = chars(0).toUpper
      new String(chars)
    }
  }
}
