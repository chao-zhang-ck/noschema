package org.datacrafts.noschema

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}
import org.datacrafts.noschema.VariableContext.LocalContext

/**
  * Base NoSchema class
  * Supports marshaling from and unmarshaling to arbitrary data structure,
  * controlled by [[Marshalling]] and [[Unmarshalling]] context
  */
abstract class NoSchema[T: NoSchema.Type](
  val category: NoSchema.Category.Value,
  val nullable: Boolean,
  val dependencies: Seq[LocalContext[_]] = Seq.empty
) extends Slf4jLogging {

  logDebug(s"constructing ${this}")

  final lazy val tpe = implicitly[NoSchema.Type[T]].tpe

  override def toString: String = s"${
    if (category == NoSchema.Category.Primitive ||
      category == NoSchema.Category.Struct) {
      tpe
    } else {
      category
    }
  }(nullable = ${nullable})"

  def marshal(input: Any, context: Marshalling[T]): T

  def unmarshal(input: T, context: Unmarshalling[T]): Any

}

object NoSchema extends Primitive.Implicits
  with Container.Implicits with ShapelessProduct.Implicits {

  // this is for future extensibility to require extra information about the type
  implicit def noSchemaType[T: TypeTag : ClassTag : Manifest]: Type[T] = new Type[T]

  class Type[T: TypeTag : ClassTag : Manifest] {
    lazy val tpe = implicitly[TypeTag[T]].tpe

    override def toString: String = s"NoSchema.Type[${tpe}]"
  }

  object Category extends Enumeration {
    val Seq, Map, Struct, Primitive, Option = Value
  }

}
