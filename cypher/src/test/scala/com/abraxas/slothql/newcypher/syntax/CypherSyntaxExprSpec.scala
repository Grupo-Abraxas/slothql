package com.abraxas.slothql.newcypher.syntax

/** Variety of [[com.abraxas.slothql.newcypher.CypherFragment.Expr]].
 *  - Input
 *  - Param
 *  - Lit
 *  - Null
 *  - ? Var
 *  - Func
 *    + built-in
 *  - MapExpr: MapDef, MapKey, MapDynKey, MapAdd
 *  - ListExpr: ListDef, InList, AtIndex, AtRange, Concat, Reduce, ListComprehension, ListPredicate (All/Any/None/Single)
 *  - StringExpr (StartsWith/EndsWith/Contains/Regex)
 *  - LogicExpr: Negate, Or, And, Xor
 *  - CompareExpr: IsNull, NotNull, Eq, Neq, Lt, Lte, Gte, Gt
 *  - MathematicalExpr: {{{unary_-}}}, {{{+}}}, {{{-}}}, {{{*}}}, {{{/}}}, {{{%}}}, {{{^}}}
 *  - Distinct
 *  - Exists
 *  - CaseExpr
 *    - SimpleCaseExpr
 *    - GenericCaseExpr
 */
class CypherSyntaxExprSpec extends CypherSyntaxBaseSpec {

}
