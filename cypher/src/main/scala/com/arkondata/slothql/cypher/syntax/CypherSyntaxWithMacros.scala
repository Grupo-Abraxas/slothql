package com.arkondata.slothql.cypher.syntax

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

import cats.data.NonEmptyList

import com.arkondata.slothql.cypher.{ CypherFragment => CF }

class CypherSyntaxWithMacros(override val c: blackbox.Context) extends CypherSyntaxPatternMacros(c) {
  import c.universe._

  def with1[T1: WeakTypeTag, R: WeakTypeTag](t1: c.Expr[CF.Expr[T1]])
           (query: c.Expr[CF.Expr.Alias[T1] => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = false, t1.tree -> weakTypeOf[T1])

  def with2[T1: WeakTypeTag, T2: WeakTypeTag, R: WeakTypeTag](t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]])
           (query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2] )=> CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = false, t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2])

  def with3[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, R: WeakTypeTag](
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = false,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3])

  def with4[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, T4: WeakTypeTag, R: WeakTypeTag](
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]], t4: c.Expr[CF.Expr[T4]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3], CF.Expr.Alias[T4]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = false,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3], t4.tree -> weakTypeOf[T4])

  def with5[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, T4: WeakTypeTag, T5: WeakTypeTag, R: WeakTypeTag](
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]], t4: c.Expr[CF.Expr[T4]], t5: c.Expr[CF.Expr[T5]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3], CF.Expr.Alias[T4], CF.Expr.Alias[T5]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = false,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3], t4.tree -> weakTypeOf[T4], t5.tree -> weakTypeOf[T5])

  def withWild0[R: WeakTypeTag](wildcard: c.Expr[**.type])(query: c.Expr[CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true)

  def withWild1[T1: WeakTypeTag, R: WeakTypeTag](wildcard: c.Expr[**.type], t1: c.Expr[CF.Expr[T1]])
               (query: c.Expr[CF.Expr.Alias[T1] => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true, t1.tree -> weakTypeOf[T1])

  def withWild2[T1: WeakTypeTag, T2: WeakTypeTag, R: WeakTypeTag](wildcard: c.Expr[**.type], t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]])
               (query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2] )=> CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true, t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2])

  def withWild3[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, R: WeakTypeTag](
    wildcard: c.Expr[**.type],
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3])

  def withWild4[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, T4: WeakTypeTag, R: WeakTypeTag](
    wildcard: c.Expr[**.type],
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]], t4: c.Expr[CF.Expr[T4]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3], CF.Expr.Alias[T4]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3], t4.tree -> weakTypeOf[T4])

  def withWild5[T1: WeakTypeTag, T2: WeakTypeTag, T3: WeakTypeTag, T4: WeakTypeTag, T5: WeakTypeTag, R: WeakTypeTag](
    wildcard: c.Expr[**.type],
    t1: c.Expr[CF.Expr[T1]], t2: c.Expr[CF.Expr[T2]], t3: c.Expr[CF.Expr[T3]], t4: c.Expr[CF.Expr[T4]], t5: c.Expr[CF.Expr[T5]]
  )(query: c.Expr[(CF.Expr.Alias[T1], CF.Expr.Alias[T2], CF.Expr.Alias[T3], CF.Expr.Alias[T4], CF.Expr.Alias[T5]) => CF.Query.Query0[R]]): c.Expr[CF.Query.Query0[R]] =
    withImpl(query.tree, wildcard = true,
      t1.tree -> weakTypeOf[T1], t2.tree -> weakTypeOf[T2], t3.tree -> weakTypeOf[T3], t4.tree -> weakTypeOf[T4], t5.tree -> weakTypeOf[T5])

  protected type ExprWithInnerType = (Tree, Type)
  protected def withImpl[R](query: Tree, wildcard: Boolean, exprs: ExprWithInnerType*): c.Expr[CF.Query.Query0[R]] = {
    val (args, body0) = query match {
      case Function(args, body) => args -> body
      case _                    => Nil  -> query
    }
    val argNames0 = args.map(_.name.toString)
    val (argNames, binds, returns) = exprs.zip(argNames0).map {
      case ((tree, tpe), nme) =>
        val name = c.freshName(nme)
        val bindName = TermName(name)
        val bind = q"val $bindName = _root_.com.arkondata.slothql.cypher.CypherFragment.Expr.Alias[$tpe]($nme)"
        val ret = q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Expr($tree, _root_.scala.Some($bindName))"
        ((nme, name), bind, ret)
    }.unzip3
    val rebind = argNames.toMap
    val return0 = (wildcard, returns) match {
      case (false, Seq())       => c.abort(c.enclosingPosition, "No variables bound at WITH clause")
      case (false, Seq(single)) => single
      case (false, seq)         => q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Tuple(_root_.scala.List(..$seq))"
      case (true, Seq())        => q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Wildcard"
      case (true, seq)          => q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.WildcardTuple(_root_.scala.List(..$seq))"
    }
    val (ops, body1) = body0 match {
      case Block(trees, tree1) =>
        val isWithSyntax = PartialFunction.cond(_: Tree) {
          case q"$obj.$_(..$_)" => obj.symbol == WithSyntax
          case q"$obj.$_"       => obj.symbol == WithSyntax
        }
        val withSyntax = trees.takeWhile(isWithSyntax)
        val rest = trees.drop(withSyntax.length)
        // Badly placed With modifiers
        rest.find(isWithSyntax).foreach{ t =>
          c.abort(t.pos, "All WITH modifiers must be defined at the beginning of the block")
        }
        val ops = withSyntax.map {
          case q"$_.where($cond)" => Opt.Where(cond)
          case q"$_.distinct"     => Opt.Distinct(q"true")
          case q"$_.distinct($b)" => Opt.Distinct(b)
          case q"$_.skip($n)"     => Opt.Skip(n)
          case q"$_.limit($n)"    => Opt.Limit(n)
          case q"$_.orderBy($expr)"       if expr.tpe <:< typeOf[CF.Expr[_]]     => Opt.OrderByExpr(expr)
          case q"$_.orderBy($expr, $ord)" if ord.tpe <:< typeOf[CF.Return.Order] => Opt.OrderByOrd(expr, ord)
          case q"$_.orderBy($expr, $sel)" if sel.tpe <:< typeOf[_ => _]          => Opt.OrderBySel(expr, sel)
          case q"$_.orderBy($seq)"        if seq.tpe <:< typeOf[Seq[_]]          => Opt.OrderBySeq(seq)
        }
        ops -> Block(rest, tree1)
      case _ =>
        Nil -> body0
    }
    val (returnTree, whereTree) = Opt.collect(ops) match {
      case Left(nel) =>
        val repeated = nel.map(_.name).toList.mkString(", ")
        val msg = s"Repeating options is not allowed: $repeated. (Only `orderBy` can be repeated)"
        c.abort(body0.pos, msg)
      case Right((whereOpt, distinctOpt, skipOpt, limitOpt, orderSeq)) =>
        val hasOptions = Seq(distinctOpt, skipOpt, limitOpt).exists(_.isDefined) || orderSeq.nonEmpty
        lazy val distinct = distinctOpt.map(_.b).getOrElse(q"false")
        lazy val orderBy = orderSeq.map {
          case Opt.OrderByExpr(expr)     => q"_root_.scala.List(($expr, _root_.com.arkondata.slothql.cypher.CypherFragment.Return.Order.Ascending))"
          case Opt.OrderByOrd(expr, ord) => q"_root_.scala.List(($expr, $ord))"
          case Opt.OrderBySel(expr, ord) => q"_root_.scala.List(($expr, $ord(_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Order)))"
          case Opt.OrderBySeq(seq)       => seq
        }.map(transformBody(rebind, _))
         .reduceLeftOption((l, r) => q"$l ++ $r")
         .getOrElse(q"_root_.scala.Nil")
        lazy val skip = q"${skipOpt.map(_.n)}"
        lazy val limit = q"${limitOpt.map(_.n)}"
        val ret = if (hasOptions) q"_root_.com.arkondata.slothql.cypher.CypherFragment.Return.Options($return0, $distinct, $orderBy, $skip, $limit)"
                  else return0
        ret -> q"${whereOpt.map(w => transformBody(rebind, w.cond))}"
    }
    c.Expr[CF.Query.Query0[R]](
      q"""
        ..$binds
        _root_.com.arkondata.slothql.cypher.CypherFragment.Query.Clause(
          _root_.com.arkondata.slothql.cypher.CypherFragment.Clause.With(
            $returnTree,
            $whereTree
          ),
          ${transformBody(rebind, body1)}
        )
      """
    )
  }

  sealed abstract class Opt(val name: String)
  object Opt {
    final case class Where(cond: Tree) extends Opt("where")
    final case class Distinct(b: Tree) extends Opt("distinct")
    final case class Skip(n: Tree)     extends Opt("skip")
    final case class Limit(n: Tree)    extends Opt("limit")
    sealed abstract class OrderBy      extends Opt("order by")
    final case class OrderByExpr(expr: Tree)           extends OrderBy
    final case class OrderByOrd(expr: Tree, ord: Tree) extends OrderBy
    final case class OrderBySel(expr: Tree, ord: Tree) extends OrderBy
    final case class OrderBySeq(seq: Tree)             extends OrderBy

    def sameType(o1: Opt, o2: Opt): Boolean = o1.getClass == o2.getClass
    def repeated(ops: Seq[Opt]): List[Opt] = repeated(ops.filterNot(isOrderBy), Nil)

    type IllegalRepeated = NonEmptyList[Opt]
    type Collect = (Option[Where], Option[Distinct], Option[Skip], Option[Limit], Seq[OrderBy])

    def collect(ops: Seq[Opt]): Either[IllegalRepeated, Collect] =
      NonEmptyList.fromList(repeated(ops)).toLeft {(
        ops.collectFirst{ case opt: Where => opt },
        ops.collectFirst{ case opt: Distinct => opt },
        ops.collectFirst{ case opt: Skip => opt },
        ops.collectFirst{ case opt: Limit => opt },
        ops.collect     { case opt: OrderBy => opt }
      )}

    @tailrec
    private def repeated(ops: Seq[Opt], acc: List[Opt]): List[Opt] = ops match {
      case h :: t if t.exists(sameType(h, _)) => repeated(t, h :: acc)
      case _ :: t => repeated(t, acc)
      case Nil => acc
    }
    private def isOrderBy = PartialFunction.cond(_: Opt) { case _: OrderBy => true }
  }

  private val WithSyntax = c.mirror.staticModule("com.arkondata.slothql.cypher.syntax.With")
}
