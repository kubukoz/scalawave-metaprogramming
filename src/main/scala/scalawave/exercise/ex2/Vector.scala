package scalawave.exercise.ex2

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 2

  The task is to implement all methods of type-sized vector. With having
  additional type information about length, we would want to enforce
  additional constraints on some methods:
  - invocation of `head` method on empty list should not compile
  - invocation of `at` method should not compile instead of throwing IndexOutOfBound
  - `::` and `++` should maintain correct size information


  See src/test/scala/scalawave/exercise/ex2/VectorSpec.scala for reference.
  Please uncomment the tests first.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex2.VectorSpec" passes

  Hints:
  - you may need to modify signatures of certain methods (like add implicit parameters)
  - using type-level computations operating on Nat from ex1 might be useful
*/

import scalawave.common.TypeLevelNat._
// you may need some Nat combinators that you defined in ex1
import scalawave.exercise.ex1._

case class Vector[N <: Nat, +T](toList: List[T]) {

  def ::[U >: T](elem: U): Vector[Succ[N], U] = ???

  // TODO: write returned type explicitly
  def ++[M <: Nat, U >: T](suffix: Vector[M, U]) = ???

  // should not compile for empty vectors
  def head: T = ???

  // should not compile if index K is out of bound given by M
  def at[K <: Nat]: T = ???

  override def toString: String =
    s"Vector[${toList.size}](${toList.mkString(", ")})"
}

object VNil extends Vector[Zero, Nothing](Nil)
