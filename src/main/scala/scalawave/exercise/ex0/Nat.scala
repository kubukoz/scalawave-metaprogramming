package scalawave.exercise.ex0

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 0

  The task is to implement all functions in Nat object that perform operations
  on natural numbers in Peano Arithmetic. You shouldn't use tail recursion, that's
  not the point of the task. Please focus on implementation making use of pattern
  matching with minimal possible cases. This has nothing to do with typelevel
  programming yet, it's only warm-up exercise. But should be useful in a moment :)

  See src/test/scala/scalawave/exercise/ex0/NatSpec.scala for reference.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex0.NatSpec" passes

  Hints:
  - use pattern matching and recursion
  - you don't need to define additional (or default) parameters
  - you don't need to define additional local functions
*/


import scala.annotation.tailrec
import scalawave.common.ValueLevelNat._

object Nat {

  def toInt(n: Nat): Int = n match {
    case Zero => 0
    case Succ(m) => 1 + toInt(m)
  }

  @tailrec
  def add(n1: Nat, n2: Nat): Nat = n1 match {
    case Zero => n2
    case Succ(m) => add(m, Succ(n2))
  }

  def lte(n1: Nat, n2: Nat): Boolean = n1 == n2 || lt(n1, n2)

  @tailrec
  def lt(n1: Nat, n2: Nat): Boolean = (n1, n2) match {
    case (Succ(a), Succ(b)) => lt(a, b)
    case (_, Zero) => false
    case (Zero, _) => true
  }

}
