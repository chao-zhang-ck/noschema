package org.datacrafts.noschema

import org.datacrafts.noschema.VariableContext.LocalContext

/**
  * Interface to implement custom marshalling and unmarshalling rules for
  * structured scala types with shapeless LabelledGeneric representation,
  * including: case classes, tuples and extensions such as scrooge thrift shapes.
  * Sophisticated custom contexts can be built to deal with all kinds of data binding issues.
  * Everything is customizable. This library only exposes the structure through shapeless.
  * Patterns like parser combinator can be implemented with customized rules
  */
trait ActionContext[T] {
  def variableContext: VariableContext[T]

  lazy val currentNode = variableContext.localContext.node
}

object ActionContext {

  trait Marshalling[T] {

    Self: ActionContext[T] =>

    @inline
    def marshal(input: Any, node: NoSchema[T]): T = {
      Option(input) match {
        case Some(value) => node.marshal(value, this)
        case None => getValueForNull()
      }
    }

    // can assign any parser here for structured type
    def getParser(): Marshalling.Parser =
      throw new Exception(s"parser for ${variableContext} not implemented")

    // can assign any decoder/deserializer here for primitive type
    @inline
    def decode(input: Any): T = input.asInstanceOf[T]

    // default null handling behavior can be overwritten
    protected def getValueForNull(): T = {
      if (currentNode.nullable) {
        if (currentNode.category == NoSchema.Category.Option) {
          None.asInstanceOf[T]
        } else {
          null.asInstanceOf[T] //scalastyle:ignore
        }
      } else {
        throw new Exception(
          s"input is null but ${currentNode} is not nullable")
      }
    }

    // default is to throw exception with unknown fields
    def allSymbolsRemoved(remainingFields: Map[String, Any]): Unit = {
      if (remainingFields.nonEmpty) {
        throw new Exception(
          s"there are unknown fields [${remainingFields.keySet.mkString(",")}], " +
            s"context=${variableContext}")
      }
    }

    // this will be automatically invoked by generated code following the structure
    def dependencyMarshalling[D](dependency: LocalContext[D]): Marshalling[D] =
      throw new Exception(s"dependency marshalling for ${variableContext} not implemented")
  }

  object Marshalling {

    trait Parser {
      def parse(input: Any): Parsed
    }

    trait Parsed {

      def removeSymbol(symbol: Symbol): Parsed

      // can control the whether the symbol is allowed to be absent and treated as null
      def getSymbolValue(symbol: Symbol): Any

      def allFields(): Map[String, Any]
    }
  }

  trait Unmarshalling[T] {
    Self: ActionContext[T] =>

    def unmarshal(input: T, node: NoSchema[T]): Any = {
      if (noOp) {input}
      else {
        Option(input) match {
          case Some(value) => node.unmarshal(value, this)
          case None => null // scalastyle:ignore
        }
      }
    }
    def getAssembler(): Unmarshalling.Assembler =
      throw new Exception(s"assembler for ${variableContext} not implemented")

    @inline
    def encode(input: T): Any = input

    def dependencyUnmarshalling[D](dependency: LocalContext[D]): Unmarshalling[D] =
      throw new Exception(s"dependency unmarshalling for ${variableContext} not implemented")

    def noOp: Boolean = false
  }

  object Unmarshalling {

    trait Assembler {
      def empty(): Assembled
    }

    trait Assembled {

      // add symbol value pairs disassembled from the structured class
      // can control the behavior when the symbol has null or empty value
      def addSymbolValue(symbol: Symbol, value: Any): Assembled

      // this controls the final output form of the structured class after reassembling
      def value: Any
    }

  }
}
