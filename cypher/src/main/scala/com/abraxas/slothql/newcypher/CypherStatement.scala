package com.abraxas.slothql.newcypher

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
  trait LiftValue[A] {
    def asParam(a: A): AnyRef
    def asLiteral(a: A): String
  }

  object LiftValue {
    implicit lazy val liftLongValue: LiftValue[Long] = new LiftValue[Long] {
      def asParam(a: Long): AnyRef = Long.box(a)
      def asLiteral(a: Long): String = a.toString
    }
  }

  final case class Part(template: String, params: Map[String, LiftValue[_]]) extends CypherStatement {
    def copy(template0: String, params0: Map[String, LiftValue[_]]): this.type = Part(template0, params0).asInstanceOf[this.type]
  }
  final case class Complete[+T](template: String, params: Map[String, LiftValue[_]]) extends CypherStatement {
    def copy(template0: String, params0: Map[String, LiftValue[_]]): this.type = Complete(template0, params0).asInstanceOf[this.type]
  }

  trait MkStatement[S] {
    def apply(template: String, params: Map[String, LiftValue[_]]): S
  }
  object MkStatement {
    implicit lazy val mkStatementPart: MkStatement[Part] = Part(_, _)
    implicit def mkCompleteStatement[T]: MkStatement[Complete[T]] = Complete(_, _)
  }

  abstract class Gen {
    def nextAlias(prefix: String): (String, Gen)
    def nextParam(prefix: String): (String, Gen)
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


    def newParam(prefix: String, lift: LiftValue[_]): GenS[Part] = GenS(
      for {
        param <- GenF(_.nextParam(prefix))
      } yield Part(param, Map(param -> lift))
    )

    def newAlias(prefix: String): GenS[Part] = GenS(GenF(_.nextAlias(prefix)).map(Part(_, Map())))
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
