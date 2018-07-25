package com.abraxas.slothql.mapper

import cats.data.NonEmptyList
import shapeless._

import com.abraxas.slothql.cypher.CypherFragment._
import com.abraxas.slothql.util.FilterNotCov


trait CypherQueryArrow extends FuncArrow {
  type Result
  type Target = Known[Query[Result]]

  override def toString: String = "<CypherQueryArrow>"
}

object CypherQueryArrow {
  type Aux[S, R] = CypherQueryArrow{ type Source = S; type Result = R }
  def apply[S, R](mkQuery: S => Known[Query[R]]): CypherQueryArrow.Aux[S, R] =
    new CypherQueryArrow {
      type Result = R
      type Source = S
      override def apply(src: Source): Known[Query[R]] = mkQuery(src)
    }

  implicit def mapGraphPathToCypherQueryArrow[A <: Arrow, R](
    implicit
    pat: PatternBuilder[A],
    ret: ReturnBuilder.Aux[A, R]
  ): Functor.Aux[A, CypherQueryArrow, CypherQueryArrow.Aux[Unit, R]] = // TODO: arrow source =================================
    Functor.define[A, CypherQueryArrow] { arr =>
      val clause = Clause.Match(NonEmptyList.of(pat(arr)), optional = false, where = None)
      val query = Query.Clause(clause, Query.Return[R](ret(arr))).known
      CypherQueryArrow(_ => query)
    }

  private lazy val theOnlyNodeAlias = "n"
  private lazy val theOnlyNodeAliasOpt: Option[String] = Some(theOnlyNodeAlias)


  trait PatternBuilder[A <: Arrow] extends DepFn1[A] { type Out = Known[Pattern.Pattern0] }
  object PatternBuilder {
    def apply[A <: Arrow](a: A)(implicit pb: PatternBuilder[A]): Known[Pattern.Pattern0] = pb(a)

    object GraphPathToCypherPattern extends Poly2 {
      implicit def mapInitialToNode[A](
        implicit
        isInitial: A <:< GraphPath.Initial[_ <: GraphRepr.Node]
      ): Case.Aux[A, Option[String], Pattern.Node] =
        at[A, Option[String]]{ (i, alias) =>
          Pattern.Node(alias = alias, labels = i.node.labels, map = Map())
        }
      implicit def mapRelationTargetToNode[A](
        implicit
        isRelationTarget: A <:< GraphPath.RelationTarget[_, _ <: GraphRepr.Node]
      ): Case.Aux[A, Option[String], Pattern.Node] =
        at[A, Option[String]]{ (tgt, alias) =>
          Pattern.Node(alias = alias, labels = tgt.node.labels, map = Map())
        }
      implicit def mapOutgoingRelationToRel[A](
        implicit
        isOutgoingRelation: A <:< GraphPath.OutgoingRelation[_, _ <: GraphRepr.Relation]
      ): Case.Aux[A, Option[String], Pattern.Rel] =
        at[A, Option[String]]( (out, alias) =>
          Pattern.Rel(
            alias = alias, types = out.relation.tpe :: Nil, map = Map(), length = None, dir = Pattern.Rel.Outgoing
          ))
    }

    implicit def buildPatternFromGraphPath[
      A <: Arrow, U0 <: HList, UR <: HList, U1 <: HList, U <: HList,
      Len <: Nat, Last <: Nat, EmptyAliases <: HList, Aliases <: HList, Ps <: HList
    ](
      implicit
      unchain: Arrow.Unchain.Aux[A, U0],
      reverse: ops.hlist.Reverse.Aux[U0, UR],
      prependInitial: InitialPrepend.Aux[UR, U1],
      dropPropSelection: FilterNotCov.Aux[U1, GraphPath.PropSelection[_, _], U],
      length: ops.hlist.Length.Aux[U, Len],
      emptyAliases: ops.hlist.Fill.Aux[Len, Option[String], EmptyAliases],
      last: ops.nat.Pred.Aux[Len, Last],
      replaceLastAlias: ops.hlist.ReplaceAt.Aux[EmptyAliases, Last, Option[String], _ <: (_, Aliases)],
      map: ops.hlist.ZipWith.Aux[U, Aliases, GraphPathToCypherPattern.type, Ps],
      buildPattern: Pattern.HBuilder[Ps]
    ): PatternBuilder[A] =
      new PatternBuilder[A] {
        private val aliases = replaceLastAlias(emptyAliases(None), theOnlyNodeAliasOpt)._2
        def apply(t: A): Known[Pattern.Pattern0] =
          buildPattern(map(dropPropSelection(prependInitial(reverse(unchain(t)))), aliases))
      }

    trait InitialPrepend[L <: HList] extends DepFn1[L] { type Out <: HList }
    object InitialPrepend {
      type Aux[L <: HList, Out0 <: HList] = InitialPrepend[L] { type Out = Out0 }

      implicit def alreadyStartsWithInitial[H, T <: HList](
        implicit
        headIsInitial: H <:< GraphPath.Initial[_]
      ): InitialPrepend.Aux[H :: T, H :: T] = instance0.asInstanceOf[InitialPrepend.Aux[H :: T, H :: T]]
      private lazy val instance0 = new InitialPrepend[HList] {
        type Out = HList
        @inline def apply(t: HList): HList = t
      }

      implicit def prependInitialToOutgoingRelation[H, T <: HList, N <: GraphRepr.Node](
        implicit
        headIsOutgoingRel: H <:< GraphPath.OutgoingRelation[N, _]
      ): InitialPrepend.Aux[H :: T, GraphPath.Initial[N] :: H :: T] =
        new InitialPrepend[H :: T] {
          type Out = GraphPath.Initial[N] :: H :: T
          def apply(t: H :: T): GraphPath.Initial[N] :: H :: T = GraphPath.Initial(t.head.node) :: t.head :: t.tail
        }

      implicit def prependInitialToPropSelection[H, T <: HList, N <: GraphRepr.Node](
        implicit
        headIsOutgoingRel: H <:< GraphPath.PropSelection[N, _]
      ): InitialPrepend.Aux[H :: T, GraphPath.Initial[N] :: H :: T] =
        new InitialPrepend[H :: T] {
          type Out = GraphPath.Initial[N] :: H :: T
          def apply(t: H :: T): GraphPath.Initial[N] :: H :: T = GraphPath.Initial(t.head.elem) :: t.head :: t.tail
        }
    }
  }

  trait ReturnBuilder[A <: Arrow] extends DepFn1[A] {
    type Result
    type Out = Known[Return[Result]]
  }
  object ReturnBuilder {
    type Aux[A <: Arrow, R] = ReturnBuilder[A] { type Result = R }
    def apply[A <: Arrow](arrow: A)(implicit rb: ReturnBuilder[A]): rb.Out = rb(arrow)

    implicit def returnLastNode[A <: Arrow, U <: HList, H](
      implicit
      unchain: Arrow.Unchain.Aux[A, U],
      head: ops.hlist.IsHCons.Aux[U, H, _],
      isNode: H <:< GraphPath.RelationArrow[_, _]
    ): ReturnBuilder.Aux[A, Map[String, Any]] =
      returnLastNodeInstance.asInstanceOf[ReturnBuilder.Aux[A, Map[String, Any]]]

    implicit def returnInitialNodeIfSingle[A <: Arrow, U <: HList, H](
      implicit
      unchain: Arrow.Unchain.Aux[A, U],
      head: ops.hlist.IsHCons.Aux[U, H, HNil],
      isInitial: H <:< GraphPath.Initial[_]
    ): ReturnBuilder.Aux[A, Map[String, Any]] =
      returnLastNodeInstance.asInstanceOf[ReturnBuilder.Aux[A, Map[String, Any]]]

    private lazy val returnLastNodeInstance = new ReturnBuilder[Arrow] {
      type Result = Map[String, Any]
      private val ret = Return.Expr(Expr.Var[Map[String, Any]](theOnlyNodeAlias), as = None).known
      def apply(t: Arrow): Known[Return.Expr[Map[String, Any]]] = ret
    }

    implicit def returnPropSelection[A <: Arrow, U <: HList, H, P <: GraphRepr.Property, R](
      implicit
      unchain: Arrow.Unchain.Aux[A, U],
      head: ops.hlist.IsHCons.Aux[U, H, _],
      isProperty: H <:< GraphPath.PropSelection[_, P],
      propertyType: P <:< GraphRepr.Property.Aux[R]
    ): ReturnBuilder.Aux[A, R] =
      new ReturnBuilder[A] {
        type Result = R
        def apply(t: A): Known[Return[R]] = {
          val nodeKey = Expr.Key(Expr.Var[Map[String, Any]](theOnlyNodeAlias), unchain(t).head.propName)
          Return.Expr(nodeKey, as = None)
        }
      }
  }
}
