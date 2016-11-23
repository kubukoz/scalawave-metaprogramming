package scalawave.exercise.ex2

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.test.illTyped

import scalawave.common.TypeLevelNat._

class VectorSpec extends WordSpec with MustMatchers {

  "Vector" when {

    "toList" should {

      "convert empty vector to empty list" in {
        VNil.toList mustBe Nil
      }

      "convert non-empty vector to corresponding list" in {
        ("alice" :: "bob" :: "cecilia" :: VNil).toList mustBe
          List("alice", "bob", "cecilia")
      }
    }

    "++" should {

      "append to empty vector" in {
        VNil ++ (1 :: 2 :: VNil) mustBe
          1 :: 2 :: VNil
      }

      "append to non-empty vector" in {
        (1 :: 2 :: 3 :: VNil) ++ (4 :: 5 :: VNil) mustBe
          (1 :: 2 :: 3 :: 4 :: 5 :: VNil)
      }
    }

    "head" should {

      "not compile for empty vector" in {
        illTyped("VNil.head")
        illTyped("(VNil ++ VNil).head")
      }

      "return first element for non-empty vector" in {
        (1 :: 2 :: VNil).head mustBe 1
        ((0 :: VNil) ++ VNil).head mustBe 0
      }
    }

    "at" should {

      type One = Succ[Zero]
      type Two = Succ[One]

      "not compile for empty vectors" in {
        illTyped("VNil.at[Zero]")
        illTyped("VNil.at[One]")
        illTyped("VNil.at[Two]")
      }

      "not compile if index out of bounds" in {
        illTyped("(1 :: VNil).at[One]")
        illTyped("(1 :: 2 :: VNil).at[Two]")
      }

      "return indexed value" in {
        val vec = 1 :: 2 :: 3 :: VNil

        vec.at[Zero] mustBe 1
        vec.at[One] mustBe 2
        vec.at[Two] mustBe 3
      }
    }

    "toString" should {

      "print elems with number of elems" in {

        VNil.toString mustBe "Vector[0]()"
        (1 :: 2 :: 3 :: VNil).toString mustBe "Vector[3](1, 2, 3)"
        ("abc" :: "xyz" :: VNil).toString mustBe "Vector[2](abc, xyz)"
      }
    }
  }
}