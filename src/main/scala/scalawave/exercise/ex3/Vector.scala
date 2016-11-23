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


import scalawave.common.TypeLevelNat._
import scalawave.exercise.ex1._
/*

trait ToList[-T <: HList, +Lub]{
  def toList(l: T): List[Lub]
}
object ToList {
  implicit val hnilList: ToList[HNil, Nothing] = (_: HNil) => Nil

  implicit def hconsList[Lub, H <: Lub, T <: HList]
  (implicit tailList: ToList[T, Lub]): ToList[H :: T, Lub] =

    (l: H :: T) => l.head :: tailList.toList(l.tail)
}
*/


case class Vector[N <: Nat, +T](toList: List[T]) {

  def ::[U >: T](elem: U): Vector[Succ[N], U] = Vector(elem :: toList)

  def ++[M <: Nat, U >: T](suffix: Vector[M, U])
                          (implicit add: Add[N, M]): Vector[add.Result, U] =
    Vector(toList ::: suffix.toList)

  // should not compile for empty vectors
  def head(implicit nz: Zero Lt N): T = toList.head

  // should not compile if index K is out of bound given by M
  def at[K <: Nat](implicit lt: K Lt N, ti: ToInt[K]): T = toList(ti.value)

  override def toString: String =
    s"Vector[${toList.size}](${toList.mkString(", ")})"
}

object VNil extends Vector[Zero, Nothing](Nil)

import shapeless.{::, Generic, HList, HNil}

object Vector {

  def apply() = VNil

  def apply[T](t: T): Vector[Succ[Zero], T] = t :: VNil
  import shapeless.ops.hlist._

  def apply[P <: Product, L <: HList, T](elem: P)
                         (implicit gen: Generic.Aux[P, L],
                          length: HlistLength[L],
                          toList: ToList[L, T])
    : Vector[length.Len, T] = {

    Vector(gen.to(elem).toList)
  }
}

trait HlistLength[T <: HList] {
  type Len <: Nat
}

object HlistLength {
  implicit val hNilLength: HlistLength[HNil] = new HlistLength[HNil] {
    override type Len = Zero
  }

  implicit def hListLength[H, Rest <: HList](implicit len: HlistLength[Rest]): HlistLength[H :: Rest] =
    new HlistLength[::[H, Rest]] {
      override type Len = Succ[len.Len]
    }
}