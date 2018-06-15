package org.datacrafts.noschema

import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}

abstract class Container[T: NoSchema.Type, C: NoSchema.Type](
  category: NoSchema.Category.Value,
  val element: VariableContext.ContainerElement[T])
  extends NoSchema[C](
    category = category,
    nullable = true,
    dependencies = Seq(element)
  )

object Container {

  trait Implicits {

    // Option is Scala specific way to deal with null.
    // This implementation unbox Options recursively.
    // Alternatively, we can treat Option same as other containers.
    // But removing Option is for better compatibility with other systems.
    // Things like Option[Option[T]] will also be eliminated naturally.
    implicit def getOptionNodeFromBaseNode[T: NoSchema.Type](implicit
      node: NoSchema[T],
      ot: NoSchema.Type[Option[T]]): Container[T, Option[T]] =
      new Container[T, Option[T]](
        category = NoSchema.Category.Option,
        element = VariableContext.ContainerElement(node)
      ) {

        // marshal input as T, and box with Option[T]
        // if T is also an Option, it'll do this recursively
        override def marshal(input: Any, context: Marshalling[Option[T]]): Option[T] = {
          // input could be Option[T] or T
          // will keep unwrap one level until it's not option any more
          // this works as long as input doesn't have more levels of option than the expected type
          val unwrapped = input match {
            case Some(value) => value
            case _ => input
          }

          Option(context.dependencyMarshalling(element).marshal(unwrapped, node))
        }

        // if input is Option[T], unmarshal T and return unboxed value
        // return null if input is None or null
        // if input is Some(null), the unmarshaller of contained element with type T
        // will handle null, which can be customized by user code
        override def unmarshal(input: Option[T], context: Unmarshalling[Option[T]]): Any = {

          Option(input).flatten.map(
            context.dependencyUnmarshalling(element).unmarshal(_, node)).getOrElse(null) // scalastyle:ignore
        }
      }

    implicit def getSeqNodeFromBaseNode[T: NoSchema.Type](implicit
      node: NoSchema[T],
      st: NoSchema.Type[Seq[T]]): Container[T, Seq[T]] =
      new Container[T, Seq[T]](
        category = NoSchema.Category.Seq,
        element = VariableContext.ContainerElement(node)
      ) {
        override def marshal(input: Any, context: Marshalling[Seq[T]]): Seq[T] =
          input match {
            case value: Iterable[_] => value.map(
              context.dependencyMarshalling(element).marshal(_, node)
            ).toSeq
            case None => throw new Exception(s"marshalling ${this} " +
              s"but input is not Iterable ${input.getClass}, ${input}")
          }

        override def unmarshal(
          input: Seq[T], context: Unmarshalling[Seq[T]]): Any =
          input.map(context.dependencyUnmarshalling(element).unmarshal(_, node))

      }

    implicit def getIterableNodeFromBaseNode[T: NoSchema.Type](implicit
      node: NoSchema[T],
      it: NoSchema.Type[Iterable[T]]): Container[T, Iterable[T]] =
      new Container[T, Iterable[T]](
        category = NoSchema.Category.Seq,
        element = VariableContext.ContainerElement(node)
      ) {
        override def marshal(input: Any, context: Marshalling[Iterable[T]]): Iterable[T] =
          input match {
            case value: Iterable[_] => value.map(
              context.dependencyMarshalling(element).marshal(_, node))
            case None => throw new Exception(s"marshalling ${this} " +
              s"but input is not Iterable ${input.getClass}, ${input}")
          }

        override def unmarshal(
          input: Iterable[T], context: Unmarshalling[Iterable[T]]): Any =
          input.map(context.dependencyUnmarshalling(element).unmarshal(_, node))
      }

    implicit def getMapNodeFromBaseNode[T: NoSchema.Type](implicit
      node: NoSchema[T],
      mt: NoSchema.Type[Map[String, T]]
    ): NoSchema[Map[String, T]] =
      new Container[T, Map[String, T]](
        category = NoSchema.Category.Map,
        VariableContext.ContainerElement(node)
      ) {
        override def marshal(
          input: Any, context: Marshalling[Map[String, T]]): Map[String, T] =
          input match {
            case value: Iterable[_] => value.map {
              case (k, v) => k.toString -> context.dependencyMarshalling(element).marshal(v, node)
            }.toMap
            case None => throw new Exception(s"marshalling ${this} " +
              s"but input is not Iterable ${input.getClass}, ${input}")
          }

        override def unmarshal(
          input: Map[String, T], context: Unmarshalling[Map[String, T]]): Any = input.map {
          case (k, v) => k -> context.dependencyUnmarshalling(element).unmarshal(v, node)
        }
      }
  }

}
