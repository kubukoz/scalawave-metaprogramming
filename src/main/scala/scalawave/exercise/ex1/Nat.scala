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

import scalawave.common.TypeLevelNat._


trait ToInt[N <: Nat] {
  def value: Int
}

object ToInt {
  def apply[N <: Nat : ToInt]: ToInt[N] = implicitly[ToInt[N]]

  // TODO: finish implementing these implicit instances

  implicit val zeroCase: ToInt[Zero] = new ToInt[Zero] {
    val value: Int = ???
  }

  implicit def succCase[N <: Nat](implicit toInt: ToInt[N]): ToInt[Succ[N]] =
    new ToInt[Succ[N]] {
      val value: Int = ???
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

  // TODO: put your implicit instances here
}


// NOTE:  We want the instances to be only for N <= M and compile error otherwise

class Lte[N <: Nat, M <: Nat]

object Lte {

  // TODO: put your implicit instances here
}


// NOTE:  We want the instances to be only for N < M and compile error otherwise

class Lt[N <: Nat, M <: Nat]

object Lt {

  // TODO: put your implicit instances here
}
