package org.datacrafts.noschema

class Primitive[T: NoSchema.ScalaType](nullable: Boolean = false)
  extends NoSchema[T](
    category = NoSchema.Category.Primitive,
    nullable = nullable
  ) {
}

object Primitive {

  trait Instances {

    implicit val intPrimitiveType = new Primitive[Int]
    implicit val shortPrimitiveType = new Primitive[Short]
    implicit val longPrimitiveType = new Primitive[Long]
    implicit val floatPrimitiveType = new Primitive[Float]
    implicit val doublePrimitiveType = new Primitive[Double]
    implicit val booleanPrimitiveType = new Primitive[Boolean]
    implicit val stringPrimitiveType = new Primitive[String](true)
    implicit val bytesPrimitiveType = new Primitive[Array[Byte]](true)
  }

}

