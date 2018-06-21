package com.abraxas.slothql

import shapeless._

import com.abraxas.slothql.FragmentDefinitionHelper._
import com.abraxas.slothql.util.Not


trait OrElse[A, B] extends DepFn2[A, B]
object OrElse {
  type Aux[A, B, Out0] = OrElse[A, B] { type Out = Out0 }
  def apply[A, B](implicit ev: A OrElse B): Aux[A, B, ev.Out] = ev

  implicit def orElseA[A, B](implicit defined: A =:!= Nothing): Aux[A, B, A] = a.asInstanceOf[Aux[A, B, A]]
  private lazy val a = new OrElse[Any, Any] {
    type Out = Any
    def apply(a: Any, b: Any): Any = a
  }

  implicit def orElseB[A, B](implicit undefined: A =:= Nothing): Aux[A, B, B] = b.asInstanceOf[Aux[A, B, B]]
  private lazy val b = new OrElse[Any, Any] {
    type Out = Any
    def apply(a: Any, b: Any): Any = b
  }
}


trait MapOrElse[HF <: Poly1, A, B] extends DepFn2[A, B]
object MapOrElse {
  type Aux[HF <: Poly1, A, B, Out0] = MapOrElse[HF, A, B] { type Out = Out0 }
  def apply[HF <: Poly1, A, B](implicit moe: MapOrElse[HF, A, B]): Aux[HF, A, B, moe.Out] = moe

  implicit def mapOrElseA[HF <: Poly1, A, B, A0](
    implicit
    defined: A =:!= Nothing,
    hf: poly.Case1.Aux[HF, A0, A]
  ): Aux[HF, A0, B, A] =
    new MapOrElse[HF, A0, B] {
      type Out = A
      def apply(a0: A0, b: B): A = hf(a0)
    }

  implicit def mapOrElseB[HF <: Poly1, A, B](implicit undefined: A =:= Nothing): Aux[HF, A, B, B] = b.asInstanceOf[Aux[HF, A, B, B]]
  private lazy val b = new MapOrElse[Poly1, Any, Any] {
    type Out = Any
    def apply(a0: Any, b: Any): Any = b
  }

}


trait Construct[A, Args <: HList] extends DepFn1[Args] { type Out <: A }
object Construct {
  type Aux[A, Args <: HList, Out0 <: A] = Construct[A, Args] { type Out = Out0 }

  def apply[A]: Builder[A] = Builder.asInstanceOf[Builder[A]]

  protected class Builder[A] extends ProductArgs {
    def applyProduct[Args <: HList](args: Args)(implicit build: Construct[A, Args]): Aux[A, Args, build.Out] = build
  }
  private object Builder extends Builder[Any]
}

trait Copy[A, Changes <: HList] extends DepFn2[A, Changes]
object Copy {
  type Aux[A, Changes <: HList, R] = Copy[A, Changes] { type Out = R }

  def apply[A](a: A): Builder[A] = new Builder[A](a)

  protected class Builder[A](a: A) extends RecordArgs {
    def applyRecord[Changes <: HList](changes: Changes)(implicit copy: Copy[A, Changes]): Aux[A, Changes, copy.Out] = copy
  }

  implicit def copyIdentity[A]: Copy.Aux[A, HNil, A] = _copyIdentity.asInstanceOf[Copy.Aux[A, HNil, A]]
  private lazy val _copyIdentity = new Copy[Any, HNil] {
    type Out = Any
    def apply(t: Any, u: HNil): Any = t
  }
}

trait Transform[A, HF <: Poly1] extends DepFn1[A]
object Transform {
  type Aux[A, HF <: Poly1, R] = Transform[A, HF] { type Out = R }

  def apply[A, HF <: Poly1](a: A, hf: HF)(implicit transform: Transform[A, HF]): Aux[A, HF, transform.Out] = transform

  @deprecated("recursion is deactivated")
  implicit def transformThis[A, HF <: Poly1, R0](implicit hf: poly.Case1.Aux[HF, A, R0]): Aux[A, HF, R0] =
    new Transform[A, HF] {
      type Out = R0
      def apply(t: A): R0 = hf(t)
    }

/* TODO: recurse
  implicit def transformThis[A, HF <: Poly1, R0](
    implicit
    hf: poly.Case1.Aux[HF, A, R0],
    next: Transform[R0, HF]
  ): Aux[A, HF, next.Out] =
    new Transform[A, HF] {
      type Out = next.Out
      def apply(t: A): next.Out = next(hf(t))
    }
*/

}


// // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //



sealed trait Expr[+T]
sealed trait Known[+T]
object LiftKnown extends Poly1 {
  implicit def impl[E <: Expr[_]]: Case.Aux[E, Known[E]] = at[E]{ _ => new Known[E] {} } // TODO: stub
}

trait Lit[A] extends Expr[A]{
  val value: A
  val m: Manifest[A]
}
object Lit{
  def apply[A: Manifest](a: A): Lit[A] =
    new Lit[A] {
      val value: A = a
      val m: Manifest[A] = manifest[A]
    }
}

//  /** ¡Template! */
//  trait Call0[A] extends Expr[A] {
//    type Func = fromNarrow[String]
//    type Params <: HList
//
//    val func: Func As String
//    val params: FromHList[Params, LiftKnown.type, List[Known[Expr[_]]]]
//  }

object Common {
  def undefined: Nothing = sys.error("undefined")
}

// Generated:

trait Call[A] extends Expr[A] {
  type Func <: String
  type Params <: HList

  val Func: Func
  val Params: Params

  val func: String
  val params: List[Known[Expr[_]]]

  // TODO:
  // - `toString`
  // - `equals`
  // - `hashCode`
}

object Call {
  type Aux[A, Func0 <: String, Params0 <: HList] = Call[A] { type Func = Func0; type Params = Params0 }
  /** Value-level constructor. */
  def apply[A](func: String, params: List[Known[Expr[_]]]): Call[A] = {
    @inline def func0 = func
    @inline def params0 = params
    new Call[A] {
      type Func = Nothing
      type Params = Nothing
      lazy val Func: Nothing = Common.undefined
      lazy val Params: Nothing = Common.undefined
      val func: String = func0
      val params: List[Known[Expr[_]]] = params0
    }
  }
  def unapply[A](arg: Expr[A]): Option[(String, List[Known[Expr[_]]])] =
    PartialFunction.condOpt(arg){ case c: Call[A @unchecked] => (c.func, c.params) }

  // Only if method `typed` isn't already defined in companion object
  /** Type-level constructor. */
  def typed[A]: Builder[A, Nothing, Nothing] = new Builder[A, Nothing, Nothing]

  // Only if method `typed` isn't already defined in companion object
  protected class Builder[A0, Func0 <: String, Params0 <: HList](
    func0: Option[String] = None,
    Func0: Option[Func0] = None,
    params0: Option[List[Known[Expr[_]]]] = None,
    Params0: Option[Params0] = None
  ) {

    // due to `Narrow` directive
    def withFunc[Func <: String](func: Func)(implicit w: Witness.Aux[Func]): Builder[A0, Func, Params0] =
      copy(func = Option(func), Func = Option(func))
    private[Call] def withFunc0[Func <: String](func: String): Builder[A0, Func, Params0] =
      copy(func = Option(func), Func = Option(func.asInstanceOf[Func]))

    // due to `FromHList` directive
    object withParams extends ProductArgs {
      def applyProduct[Params <: HList, MappedParams <: HList](params: Params)(
        implicit
        mapper: Cached[ops.hlist.Mapper.Aux[LiftKnown.type, Params, MappedParams]],
        toTraversable: Cached[ops.hlist.ToTraversable.Aux[MappedParams, List, Known[Expr[_]]]]
      ): Builder[A0, Func0, Params] =
        copy(params = Some(toTraversable.value(mapper.value(params))), Params = Option(params))
    }

    private[Call] def withParams0[Params <: HList](params: Params, asList: List[Known[Expr[_]]]): Builder[A0, Func0, Params] =
      copy(params = Option(asList), Params = Option(params))

    def build(
      implicit
      definedFunc: Func0 =:!= Nothing,
      definedParams: Params0 =:!= Nothing
    ): Call.Aux[A0, Func0, Params0] =
      new Call[A0] {
        type Func = Func0
        type Params = Params0
        val Func: Func0 = Func0 getOrElse Common.undefined
        val Params: Params0 = Params0 getOrElse Common.undefined
        val func: String = func0 getOrElse Common.undefined
        val params: List[Known[Expr[_]]] = params0 getOrElse Common.undefined
      }

    private def copy[A, Func <: String, Params <: HList](
      func: Option[String] = func0,
      Func: Option[Func] = Func0,
      params: Option[List[Known[Expr[_]]]] = params0,
      Params: Option[Params] = Params0
    ) = new Builder[A, Func, Params](func, Func, params, Params)
  }

  implicit def constructCall[A, Func0 <: String, Params0 <: HList, MappedParams <: HList](
    implicit
    w: Witness.Aux[Func0],                                                                 // TODO
    mapper: Cached[ops.hlist.Mapper.Aux[LiftKnown.type, Params0, MappedParams]],           // TODO
    toTraversable: Cached[ops.hlist.ToTraversable.Aux[MappedParams, List, Known[Expr[_]]]] // TODO
  ): Construct.Aux[Call[A], Func0 :: Params0 :: HNil, Call.Aux[A, Func0, Params0]] =
    new Construct[Call[A], Func0 :: Params0 :: HNil] {
      type Out = Call.Aux[A, Func0, Params0]
      def apply(args: Func0 :: Params0 :: HNil): Aux[A, Func0, Params0] =
        args match {
          case func :: params :: HNil =>
            Call.typed[A]
              .withFunc(func)
              .withParams.applyProduct(params)
              .build
        }
    }

  implicit def copyCallFunc[A, Func0 <: String, Params <: HList](
    implicit w: Witness.Aux[Func0]
  ): Copy.Aux[Call.Aux[A, _, Params], Witness.`'func`.Field[Func0] :: HNil, Call.Aux[A, Func0, Params]] =
    new Copy[Call.Aux[A, _, Params], Witness.`'func`.Field[Func0] :: HNil] {
      type Out = Call.Aux[A, Func0, Params]
      def apply(call: Aux[A, _, Params], args: Witness.`'func`.Field[Func0] :: HNil): Aux[A, Func0, Params] =
        Call.typed[A]
          .withFunc(args.head: Func0)
          .withParams0(call.Params, call.params)
          .build
    }

  implicit def copyCallParams[A, Func <: String, Params0 <: HList, MappedParams <: HList](
    implicit
    mapper: Cached[ops.hlist.Mapper.Aux[LiftKnown.type, Params0, MappedParams]],           // TODO
    toTraversable: Cached[ops.hlist.ToTraversable.Aux[MappedParams, List, Known[Expr[_]]]] // TODO
  ): Copy.Aux[Call.Aux[A, Func, _], Witness.`'params`.Field[Params0] :: HNil, Call.Aux[A, Func, Params0]] =
    new Copy[Call.Aux[A, Func, _], Witness.`'params`.Field[Params0] :: HNil] {
      type Out = Call.Aux[A, Func, Params0]
      def apply(call: Aux[A, Func, _], args: Witness.`'params`.Field[Params0] :: HNil): Aux[A, Func, Params0] =
        Call.typed[A]
          .withFunc0[Func](call.func)
          .withParams.applyProduct(args.head: Params0)
          .build
    }

  implicit def copyCall[A, Func0 <: String, Params0 <: HList, MappedParams <: HList](
    implicit
    w: Witness.Aux[Func0],                                                                 // TODO
    mapper: Cached[ops.hlist.Mapper.Aux[LiftKnown.type, Params0, MappedParams]],           // TODO
    toTraversable: Cached[ops.hlist.ToTraversable.Aux[MappedParams, List, Known[Expr[_]]]],// TODO
    construct: Construct.Aux[Call[A], Func0 :: Params0 :: HNil, Call.Aux[A, Func0, Params0]]
  ): Copy.Aux[Call[A], Witness.`'func`.Field[Func0] :: Witness.`'params`.Field[Params0] :: HNil, Call.Aux[A, Func0, Params0]] =
    new Copy[Call[A], Witness.`'func`.Field[Func0] :: Witness.`'params`.Field[Params0] :: HNil] {
      type Out = Call.Aux[A, Func0, Params0]
      def apply(call: Call[A], args: Witness.`'func`.Field[Func0] :: Witness.`'params`.Field[Params0] :: HNil): Aux[A, Func0, Params0] =
        construct(args)
    }


  object CallTagFuncPoly extends Poly1 {
    implicit def impl[A]: Case.Aux[A, Witness.`'func`.Field[A] :: HNil] = at[A](a => labelled.field[Witness.`'func`.T][A](a) :: HNil)
  }
  object CallTagParamsPoly extends Poly1 {
    implicit def impl[A]: Case.Aux[A, Witness.`'params`.Field[A] :: HNil] = at[A](a => labelled.field[Witness.`'params`.T][A](a) :: HNil)
  }

  implicit def transformCallChildren[A, HF <: Poly1, Func0 <: String, Func1 <: String, FuncChange <: HList, Params0 <: HList, Params1 <: HList, ParamsChange <: HList, Changes <: HList](
    implicit
    noTransform: Not[poly.Case1[HF, Call.Aux[A, Func0, Params0]]],
    transformFuncOpt: poly.Case1.Aux[HF, Func0, Func1] = null,
    transformParamsOpt: poly.Case1.Aux[HF, Params0, Params1] = null,
    func: MapOrElse.Aux[CallTagFuncPoly.type, Func1, HNil, FuncChange],
    params: MapOrElse.Aux[CallTagParamsPoly.type, Params1, HNil, ParamsChange],
    concat0: ops.hlist.Prepend.Aux[FuncChange, ParamsChange, Changes],
    copy: Copy[Call.Aux[A, Func0, Params0], Changes]
  ): Transform.Aux[Call.Aux[A, Func0, Params0], HF, copy.Out] =
    new Transform[Call.Aux[A, Func0, Params0], HF] {
      type Out = copy.Out
      def apply(call: Aux[A, Func0, Params0]): copy.Out = {
        val func0 =
          if (transformFuncOpt != null) func(transformFuncOpt(call.Func), HNil) // TODO
          else HNil.asInstanceOf[FuncChange]
        val params0 =
          if (transformParamsOpt != null) params(transformParamsOpt(call.Params), HNil)
          else HNil.asInstanceOf[ParamsChange]
        val changes = concat0(func0, params0)
        copy(call, changes)
      }
    }
}




/*
SlothQL>
val call = Call.typed[String].withFunc[Witness.`"foo"`.T]("foo").withParams(Lit("bar"), Lit("baz")).build
call: Call.Aux[String, foo, Lit[String] :: Lit[String] :: HNil] = com.abraxas.slothql.Call$Builder$$anon$9@59f03482

SlothQL> object F extends Poly1{
           implicit def impl[C <: Call[_]]: Case.Aux[C, C] = at[C](identity)
         }
defined object F

SlothQL> Transform[call.type, F.type](call, F)
res6: Transform[call.type, F.type]{type Out = com.abraxas.slothql.Call.<refinement>.type} = com.abraxas.slothql.Transform$$anon$6@5fbeb267



SlothQL> trait X
defined trait X

SlothQL>
object F extends Poly1{
  implicit def impl[C <: Call[_]]: Case.Aux[C, C with X] = at[C](_.asInstanceOf[C with X])
}
defined object F

SlothQL> Transform[call.type, F.type](call, F)
res9: Transform[call.type, F.type]{type Out = com.abraxas.slothql.Call.<refinement>.type with ammonite.$sess.cmd7.X} = com.abraxas.slothql.Transform$$anon$6@572098bb

 */