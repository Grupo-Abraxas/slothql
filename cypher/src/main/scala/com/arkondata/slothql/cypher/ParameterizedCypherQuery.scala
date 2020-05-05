package com.arkondata.slothql.cypher

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox

import shapeless._


final class ParameterizedCypherQuery[Params <: HList, T] protected(val query: CypherFragment.Query[T])
                                                                  (implicit
                                                                  val gen: CypherStatement.Gen,
                                                                  val toMap: ops.record.ToMap.Aux[Params, _ <: Symbol, _ <: Any]) {
  override def hashCode(): Int = query.hashCode()
  override def equals(obj: Any): Boolean = PartialFunction.cond(obj){
    case pcq: ParameterizedCypherQuery[_, _] => this.query == pcq.query
  }

  private[cypher] lazy val statement: query.Statement = query.toCypher(gen)._1

  override def toString: String = s"Parameterized[$query]"
}

object ParameterizedCypherQuery {

  /** Interface for building [[ParameterizedCypherQuery]] from function (with macro). */
  class Build {
    def apply(f: Any): ParameterizedCypherQuery[_, _] = macro ParameterizedCypherStatementMacros.buildImpl
  }

  def manually[Params <: HList]: ManuallyBuilder[Params] = ManuallyBuilder.asInstanceOf[ManuallyBuilder[Params]]

  class ManuallyBuilder[Params <: HList] {
    def apply[T](query: CypherFragment.Query[T])(implicit gen: CypherStatement.Gen, toMap: ops.record.ToMap.Aux[Params, _ <: Symbol, _ <: Any]): ParameterizedCypherQuery[Params, T] =
      new ParameterizedCypherQuery(query)
  }
  private object ManuallyBuilder extends ManuallyBuilder

  /** Interface for applying parameters to [[ParameterizedCypherQuery]]. */
  class Apply[Params <: HList, T, Out](pcq: ParameterizedCypherQuery[Params, T], out: CypherStatement.Prepared[T] => Out) extends RecordArgs {
    final def withParamsRecord(params: Params): Out =
      out {
        pcq.statement.withParamsUnchecked {
          pcq.toMap(params).map{ case (k, v) => k.name -> v }
        }
      }
  }

  implicit class ParameterizedCypherQueryToPreparedOps[Params <: HList, T](pcq: ParameterizedCypherQuery[Params, T]) {
    object prepared extends Apply[Params, T, CypherStatement.Prepared[T]](pcq, identity)
  }

  object Internal {
    def create[Params <: HList, T](query: CypherFragment.Query[T])
                                  (implicit toMap: ops.record.ToMap.Aux[Params, _ <: Symbol, _ <: Any]): ParameterizedCypherQuery[Params, T] =
      new ParameterizedCypherQuery(query)
  }
}

class ParameterizedCypherStatementMacros(val c: whitebox.Context) { outer =>
  import c.universe._

  private val helper = new CaseClassMacros { lazy val c: outer.c.type = outer.c }

  def buildImpl(f: Tree): Tree =
    f match {
      case Function(params, body) =>
        val (paramTrees, recTpes) = params.map{ p =>
          val tpe = p.tpt.tpe match {
            case p if p <:< typeOf[CypherFragment.Expr.Param[_]] =>
              val List(t) = p.baseType(symbolOf[CypherFragment.Expr.Param[_]]).typeArgs
              t
            case other => c.abort(p.pos, s"`parameterized` arguments must be of type `Param[?]`, got $other")
          }
          val argTree =
            q"""
              _root_.com.arkondata.slothql.cypher.CypherFragment.Expr.Param[$tpe](
                ${p.name.decodedName.toString},
                _root_.com.arkondata.slothql.cypher.CypherStatement.LiftValue[$tpe]
              )
            """
          val recEntry = helper.mkFieldTpe(p.name, tpe)
          argTree -> recEntry
        }.unzip
        val recTpe = helper.mkHListTpe(recTpes)
        val List(retType) = body.tpe.baseType(symbolOf[CypherFragment.Query[_]]).typeArgs

        q"""
          _root_.com.arkondata.slothql.cypher.ParameterizedCypherQuery.Internal.create[$recTpe, $retType](
            $f(..$paramTrees)
          ): _root_.com.arkondata.slothql.cypher.ParameterizedCypherQuery[$recTpe, $retType]
         """
      case _ =>
        c.abort(c.enclosingPosition, "Expecting function (Expr.Param[A1], Expr.Param[A2], ...) => Query.Query0[R]")
    }

}
