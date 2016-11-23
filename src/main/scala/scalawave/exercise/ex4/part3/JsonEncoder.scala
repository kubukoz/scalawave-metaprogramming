package scalawave.exercise.ex4.part3

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 4, part III

  This exercise is continuation of typeclass derivation task for JsonEncoder. In this
  part we focus on providing instances for algebraic data types (simple sealed trait
  hierarchies).

  See src/test/scala/scalawave/exercise/ex4/part3/JsonEncoderSpec.scala for reference.

  The idea is to use shapeless encoding for coproducts and derive instances for them.

  Please uncomment the tests first.

  Good luck.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex4.part3.JsonEncoderSpec" passes

  Hints:
  - you may re-use your solution from part II
  - what is LabelledGeneric representation of scala coproducts (eithers or algebraic data types)?
  - provide type-class instances for shapeless coproducts (CNil and :+:)
*/

import shapeless._
import shapeless.labelled._

import scalawave.common.Json


trait JsonEncoder[T] {
  type Out <: Json.JsValue
  def toJson(obj: T): Out
}

object JsonEncoder {
  import Json._

  type Aux[T, O <: JsValue] = JsonEncoder[T] { type Out = O }

  def apply[T](implicit enc: JsonEncoder[T]): JsonEncoder[T] = enc

  def makeEnc[T, O <: JsValue](enc: T => O): JsonEncoder.Aux[T, O] = new JsonEncoder[T] {
    type Out = O
    def toJson(obj: T): O = enc(obj)
  }


  implicit val intEncoder: Aux[Int, JsNum] = makeEnc[Int, JsNum](JsNum(_))

  implicit val strEncoder: Aux[String, JsStr] = makeEnc[String, JsStr](JsStr)

  implicit val boolEncoder: Aux[Boolean, JsBool] = makeEnc[Boolean, JsBool](JsBool)

  implicit val doubleEncoder: Aux[Double, JsNum] = makeEnc[Double, JsNum](JsNum(_))

  implicit val hnilCase: Aux[HNil, JsObj] = makeEnc[HNil, JsObj](_ => JsObj())

  implicit def hconsCase[H, T <: HList, K <: Symbol](
                                                      implicit hEnc: JsonEncoder[H],
                                                      tEnc: JsonEncoder.Aux[T, JsObj],
                                                      wit: Witness.Aux[K]
                                                    ): JsonEncoder.Aux[FieldType[K, H] :: T, JsObj] = makeEnc {
    case h :: t =>
      (wit.value.name -> hEnc.toJson(h)) +: tEnc.toJson(t)
  }

  implicit val cNilEnc = makeEnc[CNil, JsObj](_ => throw new RuntimeException("will never happen"))

  implicit def cConsCase[N <: Symbol, H, T <: Coproduct](
                                                        implicit nameWit: Witness.Aux[N],
                                                        hEnc: JsonEncoder[H],
                                                        tEnc: JsonEncoder.Aux[T, JsObj]
                                                        ) = makeEnc[FieldType[N, H] :+: T, JsObj]{
    case Inl(head) => JsObj(nameWit.value.name -> hEnc.toJson(head))
    case Inr(tail) => tEnc.toJson(tail)
  }


  implicit def genInstance[T, R](
                                  implicit gen: LabelledGeneric.Aux[T, R],
                                  encoder: JsonEncoder.Aux[R, JsObj]
                                ) =
    makeEnc[T, JsObj]{ value =>
      encoder.toJson(gen.to(value))
    }

}

object JsonEncoderSyntax {

  implicit class JsonEncoderOps[T, O <: Json.JsValue](val obj: T) extends AnyVal {
    def toJson(implicit enc: JsonEncoder.Aux[T, O]): O = enc.toJson(obj)
    def toJsonString(implicit enc: JsonEncoder.Aux[T, O]) = Json.toString(obj.toJson)
  }
}
