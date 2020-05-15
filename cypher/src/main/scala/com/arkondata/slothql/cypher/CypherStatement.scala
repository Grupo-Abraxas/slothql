package com.arkondata.slothql.cypher

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.JavaConverters
import scala.language.{ higherKinds, implicitConversions }

import cats.{ Applicative, Eval, Foldable, Functor, Monad, MonoidK, Semigroupal, StackSafeMonad, catsInstancesForId }
import cats.arrow.Arrow
import cats.instances.function._
import cats.instances.map._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroupk._


sealed trait CypherStatement {
  val template: String
  val params: Map[String, CypherStatement.LiftValue[_]]

  def copy(template: String = this.template, params: Map[String, CypherStatement.LiftValue[_]] = this.params): this.type
}

object CypherStatement {

  final case class Prepared[R](template: String, params: Map[String, AnyRef])

  trait LiftValue[A] {
    def asParam(a: A): AnyRef
    def asLiteral(a: A): String
  }

  object LiftValue {
    def apply[A](implicit lift: LiftValue[A]): LiftValue[A] = lift

    implicit lazy val liftLongValue: LiftValue[Long] = new LiftValue[Long] {
      def asParam(a: Long): AnyRef = Long.box(a)
      def asLiteral(a: Long): String = a.toString
    }
    implicit lazy val liftIntValue: LiftValue[Int] = new LiftValue[Int] {
      def asParam(a: Int): AnyRef = Int.box(a)
      def asLiteral(a: Int): String = a.toString
    }
    implicit lazy val liftBooleanValue: LiftValue[Boolean] = new LiftValue[Boolean] {
      def asParam(a: Boolean): AnyRef = Boolean.box(a)
      def asLiteral(a: Boolean): String = a.toString
    }
    implicit lazy val liftStringValue: LiftValue[String] = new LiftValue[String] {
      def asParam(a: String): AnyRef = a
      def asLiteral(a: String): String = s""""${a.replaceAll("\"", "\\\"")}""""
    }
    implicit def liftOptionValue[T](implicit lift: LiftValue[T]): LiftValue[Option[T]] = new LiftValue[Option[T]] {
      def asParam(a: Option[T]): AnyRef = a.map(lift.asParam).orNull
      def asLiteral(a: Option[T]): String = a.map(lift.asLiteral).orNull
    }
    implicit def liftListValue[T](implicit lift: LiftValue[T]): LiftValue[List[T]] = new LiftValue[List[T]] {
      import JavaConverters._
      def asParam(a: List[T]): AnyRef = a.map(lift.asParam).asJava
      def asLiteral(a: List[T]): String = a.map(lift.asLiteral).mkString("[", ", ", "]")
    }
    implicit def liftStringMapValue[T](implicit lift: LiftValue[T]): LiftValue[Map[String, T]] = new LiftValue[Map[String, T]] {
      import JavaConverters._
      def asParam(a: Map[String, T]): AnyRef = a.mapValues(lift.asParam).asJava
      def asLiteral(a: Map[String, T]): String = literalMap(a.mapValues(lift.asLiteral).toMap)
    }

    implicit def liftStringMapLiftedValues[L <: LiftedValue]: LiftValue[Map[String, L]] =
      _liftStringMapLiftedValues.asInstanceOf[LiftValue[Map[String, L]]]
    private lazy val _liftStringMapLiftedValues: LiftValue[Map[String, LiftedValue]] = new LiftValue[Map[String, LiftedValue]] {
      import JavaConverters._
      def asParam(a: Map[String, LiftedValue]): AnyRef = a.mapValues(v => v.lift.asParam(v.value)).toMap.asJava
      def asLiteral(a: Map[String, LiftedValue]): String = literalMap(a.mapValues(v => v.lift.asLiteral(v.value)).toMap)
    }

    private def literalMap(m: Map[String, String]) = m
      .map{ case (k, v) => s"${CypherFragment.escapeName(k)}: $v" }
      .mkString("{", ", ", "}")
  }

  trait LiftedValue {
    type Value
    val value: Value
    val lift: LiftValue[Value]
  }

  final case class Part(template: String, params: Map[String, LiftValue[_]]) extends CypherStatement {
    def copy(template0: String, params0: Map[String, LiftValue[_]]): this.type = Part(template0, params0).asInstanceOf[this.type]
  }

  final case class Complete[+T](template: String, params: Map[String, LiftValue[_]]) extends CypherStatement {
    def copy(template0: String, params0: Map[String, LiftValue[_]]): this.type = Complete(template0, params0).asInstanceOf[this.type]

    def withParams(params: Map[String, Any]): Prepared[T @ uncheckedVariance] =
      withParamsUnchecked(params)
        .ensuring(this.params.keys == params.keys,
                  s"Wrong query parameters: got ${params.keys.mkString(", ")}; expected: ${this.params.keys.mkString(", ")}")

    private[cypher] def withParamsUnchecked(params: Map[String, Any]): Prepared[T @ uncheckedVariance] = {
      Prepared(
        template,
        this.params.map { case (k, lift: CypherStatement.LiftValue[Any]) => k -> lift.asParam(params(k)) }
      )
    }
  }

  trait MkStatement[S] {
    def apply(template: String, params: Map[String, LiftValue[_]]): S
  }
  object MkStatement {
    implicit lazy val mkStatementPart: MkStatement[Part] = Part(_, _)
    implicit def mkCompleteStatement[T]: MkStatement[Complete[T]] = Complete(_, _)
  }

  trait Alias {
    def name: String
  }

  object Alias {
    trait Fixed extends Alias
  }

  trait Param {
    def name: String
  }

  abstract class Gen {
    def nextAlias(alias: Alias): (String, Gen)
    def nextParam(param: Param): (String, Gen)
  }
  object Gen {
    type Hash = Int
    type Suffix = Int

    final class Default protected (
      hashedAliases: Map[Hash, String],
      usedAliases: Map[String, Suffix],
      hashedParams: Set[Hash],
      usedParams: Set[String]
    ) extends Gen {
      def nextAlias(alias: Alias): (String, Gen) =
        hashedAliases.get(alias.##)
          .map(_ -> this)
          .getOrElse {
            val s = alias.name.trim
            val suffix = usedAliases.getOrElse(s, 0)
            val s1 = s"$s$suffix"
            s1 -> new Default(hashedAliases.updated(alias.##, s1), usedAliases.updated(s, suffix + 1), hashedParams, usedParams)
          }
      def nextParam(param: Param): (String, Gen) = () match {
        case _ if hashedParams contains param.## => param.name -> this
        case _ if usedParams contains param.name => sys.error(s"Parameter '$param' has already been used")
        case _ => param.name -> new Default(hashedAliases, usedAliases, hashedParams + param.##, usedParams + param.name)
      }
    }
    object Default{
      def apply(): Default = new Default(Map(), Map(), Set(), Set())
    }

    implicit def default: Gen = Default()
  }

  final class GenF[A](val f: Gen => (A, Gen)) extends AnyVal {
    @inline def apply(gen: Gen): (A, Gen) = f(gen)

    def map[B]    (g: A => B      ): GenF[B] = new GenF(f andThen Arrow[Function1].first(g))
    def flatMap[B](g: A => GenF[B]): GenF[B] = new GenF(f andThen Function.uncurried(g andThen (_.f)).tupled)

  }
  object GenF {
    @inline def apply[A](f: Gen => (A, Gen)): GenF[A] = new GenF(f)
    @inline def pure[A](a: A): GenF[A] = new GenF((a, _))

    implicit lazy val GenFMonad: Monad[GenF] = new StackSafeMonad[GenF] {
      @inline override def map[A, B](fa: GenF[A])(f: A => B): GenF[B] = fa.map(f)
      @inline def pure[A](x: A): GenF[A] = GenF.pure(x)
      @inline def flatMap[A, B](fa: GenF[A])(f: A => GenF[B]): GenF[B] = fa.flatMap(f)
    }

    implicit lazy val GenFSemigroupal: Semigroupal[GenF] = new Semigroupal[GenF] {
      def product[A, B](fa: GenF[A], fb: GenF[B]): GenF[(A, B)] =
        new GenF(gen => {
          val (a, gen1) = fa(gen)
          val (b, gen2) = fb(gen1)
          (a, b) -> gen2
        })
    }

    def sequence[C[_]: Applicative: Foldable, A](fs: C[GenF[A]])(implicit M: MonoidK[C]): GenF[C[A]] = new GenF(
      fs.foldRight(Eval.now((M.empty[A], _: Gen))) {
        case (f, accF) => accF.map(_ andThen {
          case (acc, gen) => f.f andThen Arrow[Function1].first(_.pure[C] <+> acc) apply gen
        })
      }.value
    )
  }

  final class GenS0[C[_]: Functor: Foldable, S <: CypherStatement](val f: GenF[C[S]])(implicit mk: MkStatement[S]) {
    def map(g: C[String] => String) : GenS[S] = GenS(
      f.map { cs =>
        mk(g(cs.map(_.template)), cs.map(_.params).foldK)
      }
    )
    def flatMap(g: C[String] => GenS[S]): GenS[S] = GenS0(
      f.flatMap{ cs =>
        g(cs.map(_.template)).f.map(s => s.copy(params = cs.map(_.params).foldK ++ s.params))
      }
    )
  }

  object GenS0 {
    def apply[C[_]: Functor: Foldable, S <: CypherStatement: MkStatement](gen: GenF[C[S]]): GenS0[C, S] = new GenS0(gen)

    implicit class GenSTuple2Ops[S <: CypherStatement](gens: (GenS[S], GenS[S]))(implicit mk: MkStatement[S]) {
      def map2(f: (String, String) => String): GenS[S] = GenS((gens._1.f, gens._2.f).mapN(
        (s1, s2) =>
          mk(f(s1.template, s2.template), s1.params ++ s2.params)
      ))
    }
  }


  type GenS[S <: CypherStatement] = GenS0[cats.Id, S]
  object GenS {
    def apply[S <: CypherStatement: MkStatement](f: GenF[S]): GenS[S] = new GenS(f)
    def raw[S <: CypherStatement: MkStatement](f: Gen => (S, Gen)): GenS[S] = new GenS(new GenF(f))

    def part(k: CypherFragment.Aux[Part]): GenS[Part] = raw(k.toCypher(_))
    def part(template: String): GenS[Part] = raw((Part(template, Map.empty), _))

    def complete[T](k: CypherFragment.Aux[Complete[T]]): GenS[Complete[T]] = raw(k.toCypher(_))
    def complete[T](template: String): GenS[Complete[T]] = raw((Complete(template, Map.empty), _))

    def sequence[C[_]: Applicative: Foldable: MonoidK, S <: CypherStatement: MkStatement](fs: C[GenS[S]]): GenS0[C, S] =
      GenS0(GenF.sequence(fs.map(_.f)))

    def partsSequence[C[_]: Applicative: Foldable](fs: C[_ <: CypherFragment.Aux[Part]])
                                                  (implicit M: MonoidK[C]): GenS0[C, Part] =
      sequence(fs.map(part))

    def completeSequence[A]: CompleteSequenceBuilder[A] = CompleteSequenceBuilder.asInstanceOf[CompleteSequenceBuilder[A]]

    protected class CompleteSequenceBuilder[A] {
      def apply[C[+_]: Applicative: Foldable](fs: C[CypherFragment.Aux[Complete[_]]])
                                             (implicit M: MonoidK[C]): GenS0[C, Complete[A]] =
        sequence(fs.map(complete[Any])).asInstanceOf[GenS0[C, Complete[A]]]
    }
    private object CompleteSequenceBuilder extends CompleteSequenceBuilder[Any]


    def liftParam(param: Param, lift: LiftValue[_]): GenS[Part] = GenS(
      for {
        param <- GenF(_.nextParam(param))
      } yield Part(param, Map(param -> lift))
    )

    def liftAlias(alias: Alias): GenS[Part] = liftAlias(Option(alias))
    def liftAlias(opt: Option[Alias]): GenS[Part] = opt
      .map{
        case fixed: Alias.Fixed => part(CypherFragment.escapeName(fixed.name))
        case wildcard if wildcard.name == "_" => part("")
        case alias => GenS(GenF(_.nextAlias(alias)).map(a => Part(CypherFragment.escapeName(a), Map())))
      }
      .getOrElse(part(""))
  }

  implicit class GenSPartOps[C[_]: Functor: Foldable](gf: GenS0[C ,Part]) {
    def toComplete[T]: GenS0[C, Complete[T]] = GenS0(gf.f.map(_.map{ case Part(template, params) => Complete[T](template, params) }))
  }

  implicit class ToGenSOps[C[_]: Functor: Foldable, S <: CypherStatement: MkStatement](f: GenF[C[S]]) {
    def genS: GenS0[C, S] = new GenS0(f)
  }
  implicit class ToGenSIdOps[S <: CypherStatement: MkStatement](f: GenF[S]) {
    def genS: GenS[S] = new GenS[S](f)
  }
}
