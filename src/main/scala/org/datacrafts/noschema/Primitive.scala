package org.datacrafts.noschema

import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}

class Primitive[T: NoSchema.Type](nullable: Boolean = false)
  extends NoSchema[T](
    category = NoSchema.Category.Primitive,
    nullable = nullable) {

  def marshal(input: Any, context: Marshalling[T]): T = {
    context.decode(input)
  }

  override def unmarshal(input: T, context: Unmarshalling[T]): Any = {
    context.encode(input)
  }
}

object Primitive {

  trait Implicits {

    implicit val intPrimitiveType = new Primitive[Int]
    implicit val longPrimitiveType = new Primitive[Long]
    implicit val floatPrimitiveType = new Primitive[Float]
    implicit val doublePrimitiveType = new Primitive[Double]
    implicit val stringPrimitiveType = new Primitive[String](true)
    implicit val bytesPrimitiveType = new Primitive[Array[Byte]](true)
  }

}
