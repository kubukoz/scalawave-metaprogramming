package scalawave.exercise.ex4.part4

/*
  ScalaWave Typelevel Metaprogramming Workshop
  Exercise 4, part IV

  This exercise is continuation of typeclass derivation task for JsonEncoder. In this
  part we focus on providing support for nested products, recursive data structures and
  more convenient encoding of standard scala types like Option, List and Map.

  See src/test/scala/scalawave/exercise/ex4/part4/JsonEncoderSpec.scala for reference.

  Please uncomment the tests first.

  Some of them will not pass due to divergent implicit error. The idea is to use shapeless.Lazy
  in certain places to defer resolving implicits.

  Good luck.

  Your solution is ready when:
  - sbt "testOnly scalawave.exercise.ex4.part4.JsonEncoderSpec" passes

  Hints:
  - re-use your solution from part III
  - where do you need to put shapeless.Lazy?
  - define direct implicit instances for Option, List, Map
*/

import shapeless._
import shapeless.labelled._

import scalawave.common.Json
import scalawave.common.Json.{JsObj, JsValue}


trait JsonEncoder[T] {
  type Out <: Json.JsValue

  def toJson(obj: T): Out
}

object JsonEncoder extends LowPriorityImplicits {

  import Json._

  type Aux[T, O <: JsValue] = JsonEncoder[T] {type Out = O}

  def apply[T](implicit enc: JsonEncoder[T]): JsonEncoder[T] = enc


  implicit val intEncoder: Aux[Int, JsNum] = makeEnc[Int, JsNum](JsNum(_))

  implicit val strEncoder: Aux[String, JsStr] = makeEnc[String, JsStr](JsStr)

  implicit val boolEncoder: Aux[Boolean, JsBool] = makeEnc[Boolean, JsBool](JsBool)

  implicit val doubleEncoder: Aux[Double, JsNum] = makeEnc[Double, JsNum](JsNum(_))

  implicit val hnilCase: Aux[HNil, JsObj] = makeEnc[HNil, JsObj](_ => JsObj())

  implicit def hconsCase[H, T <: HList, K <: Symbol](
                                                      implicit hEnc: JsonEncoder[H],
                                                      tEnc: Lazy[JsonEncoder.Aux[T, JsObj]],
                                                      wit: Witness.Aux[K]
                                                    ): JsonEncoder.Aux[FieldType[K, H] :: T, JsObj] = makeEnc {
    case h :: t =>
      (wit.value.name -> hEnc.toJson(h)) +: tEnc.value.toJson(t)
  }

  implicit val cNilEnc = makeEnc[CNil, JsObj](_ => throw new RuntimeException("will never happen"))


  implicit def cConsCase[N <: Symbol, H, T <: Coproduct](
                                                          implicit nameWit: Witness.Aux[N],
                                                          hEnc: Lazy[JsonEncoder[H]],
                                                          tEnc: JsonEncoder.Aux[T, JsObj]
                                                        ) = makeEnc[FieldType[N, H] :+: T, JsObj] {
    case Inl(head) => JsObj(nameWit.value.name -> hEnc.value.toJson(head))
    case Inr(tail) => tEnc.toJson(tail)
  }

  implicit def optJson[T](implicit enc: JsonEncoder[T]): Aux[Option[T], JsValue] = makeEnc[Option[T], JsValue] {
    case None => JsNull
    case Some(a) => enc.toJson(a)
  }

  implicit def listJson[T](implicit enc: JsonEncoder[T]) = makeEnc[List[T], JsArr] {
    v => JsArr(v.map(elem => enc.toJson(elem)))
  }

  implicit def mapJson[V](implicit enc: JsonEncoder[V]) = makeEnc[Map[String, V], JsObj] { map =>
    JsObj(map.map { case (key, value) =>
      key -> enc.toJson(value)
    }.toList)
  }

}

trait LowPriorityImplicits {

  def makeEnc[T, O <: JsValue](enc: T => O): JsonEncoder.Aux[T, O] = new JsonEncoder[T] {
    type Out = O

    def toJson(obj: T): O = enc(obj)
  }

  implicit def genInstance[T, R](
                                  implicit gen: LabelledGeneric.Aux[T, R],
                                  encoder: Lazy[JsonEncoder.Aux[R, JsObj]]
                                ): JsonEncoder.Aux[T, JsObj] =
    makeEnc[T, JsObj] { value =>
      encoder.value.toJson(gen.to(value))
    }

}

object JsonEncoderSyntax {

  implicit class JsonEncoderOps[T, O <: Json.JsValue](val obj: T) extends AnyVal {
    def toJson(implicit enc: JsonEncoder.Aux[T, O]): O = enc.toJson(obj)

    def toJsonString(implicit enc: JsonEncoder.Aux[T, O]) = Json.toString(obj.toJson)
  }

}
