package scalawave.exercise.ex1

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 1

  The task is to implement all typelevel computations, that performs the same
  calculations on natural numbers like functions in exercise 0.
  You need to satisfy requirements by providing implicit instances for
  recursive schemas defined here. The intent of this task is to compare
  value-level programming with type-level one.

  See src/test/scala/scalawave/exercise/ex1/NatSpec.scala for reference.
  Please uncomment the tests first.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex1.NatSpec" passes

  Hints:
  - use recursion on implicits

  NOTE:

    Combinators like

    def apply[N <: Nat, M <: Nat](implicit add: Add[N, M]) = new {
      def toInt(implicit toInt: ToInt[add.Result]): Int = toInt.value
    }

    are defined to bring convenient syntax for performing type-level computation
    and bringing the result to the value level. It might be used like:

    Add[Zero, Succ[Zero]].toInt // should be 1: Int
*/

import shapeless.=:!=

import scalawave.common.TypeLevelNat._


trait ToInt[N <: Nat] {
  def value: Int
}

object ToInt {
  def apply[N <: Nat : ToInt]: ToInt[N] = implicitly[ToInt[N]]

  // TODO: finish implementing these implicit instances

  implicit val zeroCase: ToInt[Zero] = new ToInt[Zero] {
    val value: Int = 0
  }

  implicit def succCase[A <: Nat](implicit toInt: ToInt[A]): ToInt[Succ[A]] =
    new ToInt[Succ[A]] {
      val value: Int = 1 + toInt.value
    }
}


trait Add[N <: Nat, M <: Nat] {
  type Result <: Nat
}

object Add {
  type Aux[N <: Nat, M <: Nat, R <: Nat] = Add[N, M] { type Result = R }

  def apply[N <: Nat, M <: Nat](implicit add: Add[N, M]) = new {
    def toInt(implicit toInt: ToInt[add.Result]): Int = toInt.value
  }

  implicit def zeroAdd[M <: Nat]: Add.Aux[Zero, M, M] = new Add[Zero, M] {
    override type Result = M
  }

  implicit def succAdd[N <: Nat, M <: Nat](implicit add: Add[N, Succ[M]]): Add.Aux[Succ[N], M, add.Result] = new Add[Succ[N], M] {
    override type Result = add.Result
  }
}


// NOTE:  We want the instances to be only for N <= M and compile error otherwise

class Lte[N <: Nat, M <: Nat]

object Lte {

  implicit def succCase[N <: Nat, M <: Nat](implicit lte1: Lte[N, M]): Lte[Succ[N], Succ[M]] = new Lte[Succ[N], Succ[M]]

  implicit def zeroLCase[M <: Nat]: Lte[Zero, M] = new Lte[Zero, M]

}

// NOTE:  We want the instances to be only for N < M and compile error otherwise

class Lt[N <: Nat, M <: Nat]

object Lt {
  implicit def lt[N <: Nat, M <: Nat](implicit neq: N =:!= M,
                                      lte: Lte[N, M]): Lt[N, M] = new Lt[N, M]
}
