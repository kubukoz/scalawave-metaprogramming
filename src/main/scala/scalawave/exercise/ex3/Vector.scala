package scalawave.exercise.ex3

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 3

  The task is to implement convenient constructor method (Vector.apply) that
  takes arbitrary number of elements and constructs the Vector instance keeping
  the size information.

  The idea is to define this method for product types (which tuples are),
  convert it to shapeless generic product representation (HList), compute its
  type-level length and convert to standard scala List. Having last two, we can
  construct our Vector. Good luck :)

  See src/test/scala/scalawave/exercise/ex3/VectorApplySpec.scala for reference.
  Please uncomment the tests first.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex3.VectorApplySpec" passes

  Hints:
  - use shapeless HList, Generic, ToList
  - consider how to compute length of HList (you should be able to do this)
  - don't import shapeless.Nat as we don't use shapeless natural number representation
    in this task, but our own defined in ex1
*/

import shapeless.ops.hlist.ToList
import shapeless.{::, Generic, HList, HNil}

import scalawave.common.TypeLevelNat._
import scalawave.exercise.ex1._

case class Vector[N <: Nat, +T](toList: List[T]) {

  // TODO: copy your solution from ex2

  def ::[U >: T](elem: U): Vector[Succ[N], U] = ???
  def ++[M <: Nat, U >: T](suffix: Vector[M, U]) = ???
  def head: T = ???
  def at[K <: Nat]: T = ???

  override def toString: String =
    s"Vector[${toList.size}](${toList.mkString(", ")})"
}

object VNil extends Vector[Zero, Nothing](Nil)

object Vector {

  def apply[T]() = ???
}
