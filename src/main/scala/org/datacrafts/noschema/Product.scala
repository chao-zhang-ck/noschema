package org.datacrafts.noschema

import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}
import org.datacrafts.noschema.ActionContext.Marshalling.Parsed
import org.datacrafts.noschema.ActionContext.Unmarshalling.Assembled
import org.datacrafts.noschema.Product.Shapeless
import org.datacrafts.noschema.VariableContext.LocalContext
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled._

/**
  * Product type by using shapeless
  */
class Product[T : NoSchema.Type, R <: HList](
  dependencies: Seq[LocalContext[_]],
  generic: LabelledGeneric.Aux[T, R],
  shapeless: Shapeless[R]) extends NoSchema[T](
  category = NoSchema.Category.Struct,
  nullable = true,
  dependencies = dependencies
) {

  override def marshal(input: Any, context: Marshalling[T]): T = {
    generic.from(shapeless.marshalHList(context.getParser().parse(input), context))
  }

  override def unmarshal(input: T, context: Unmarshalling[T]): Any = {
    shapeless.unmarshalHList(generic.to(input), context).value
  }
}

object Product {

  trait FallbackImplicits {

    // this helps pass compile time check, and will throw runtime error
    // if certain type transformer is missing
    implicit def lowPriorityFallBackImplicit[K <: Symbol, V: NoSchema.Type, L <: HList](implicit
      w: Witness.Aux[K],
      l: Shapeless[L]
    ): Shapeless[FieldType[K, V] :: L] = {
      throw new Exception(s"field ${w.value.name} of ${implicitly[NoSchema.Type[V]]} " +
        s"does not have NoSchema in scope.")
    }

  }

  trait Implicits extends FallbackImplicits {

    implicit val hNilNode = new Shapeless[HNil](Seq.empty) {
      override def marshalHList(parsed: Parsed, context: Marshalling[_]) = {
        context.allSymbolsRemoved(parsed.allFields())
        HNil
      }

      override def unmarshalHList(hList: HNil, context: Unmarshalling[_]) = {
        context.getAssembler.empty()
      }
    }

    implicit def shapelessRecursiveBuilder[K <: Symbol, V, L <: HList](implicit
      headSymbol: Witness.Aux[K],
      headValue: Lazy[NoSchema[V]],
      tail: Lazy[Shapeless[L]]
    ): Shapeless[FieldType[K, V] :: L] = {

      val headValueContext =
        VariableContext.MemberVariable(Some(headSymbol.value), headValue.value)

      new Shapeless[FieldType[K, V] :: L](members = tail.value.members :+ headValueContext) {

        override def marshalHList(parsed: Parsed, context: Marshalling[_]): FieldType[K, V] :: L = {
          field[K](
            context.dependencyMarshalling(headValueContext).marshal(
              input = parsed.getSymbolValue(headSymbol.value),
              node = headValue.value)
          ) :: tail.value.marshalHList(parsed.removeSymbol(headSymbol.value), context)
        }

        override def unmarshalHList(
          hList: FieldType[K, V] :: L, context: Unmarshalling[_]): Assembled = {
          tail.value.unmarshalHList(hList.tail, context)
            .addSymbolValue(
              symbol = headSymbol.value,
              value = context.dependencyUnmarshalling(headValueContext).unmarshal(
                input = hList.head, node = headValue.value
              )
            )
        }
      }
    }

    implicit def shapelessBridging[T: NoSchema.Type, R <: HList](implicit
      generic: LabelledGeneric.Aux[T, R],
      shapeless: Lazy[Shapeless[R]]
    ): NoSchema[T] = shapeless.value.composeWithGeneric(generic)
  }

  /**
    * 1. can perform recursive construction/destruction of HList
    * 2. can compose Product type with labelledGeneric bridging
    */
  abstract class Shapeless[R <: HList](
    val members: Seq[VariableContext.MemberVariable[_]]) {

    def marshalHList(parsed: Parsed, context: Marshalling[_]): R

    def unmarshalHList(hList: R, context: Unmarshalling[_]): Assembled

    def composeWithGeneric[T: NoSchema.Type](
      generic: LabelledGeneric.Aux[T, R]): NoSchema[T] = new Product(members, generic, this)
  }
}
