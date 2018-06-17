package org.datacrafts.noschema.schema

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchema, VariableContext}
import org.datacrafts.noschema.ActionContext.Marshalling.{Parsed, Parser}
import org.datacrafts.noschema.ActionContext.Unmarshalling.{Assembled, Assembler}

/**
  * Marshalling from and unmarshalling to Map[String, Any]
  */
object MapSchema {

  def default[T: NoSchema]: Schema[T] =
    SimpleDecider.getSchema(VariableContext.root(implicitly[NoSchema[T]]))

  // although decider can do arbitrary things, this is recommended way to pass on the same
  // decider object along.
  // This decider object is the single place to define an entire set of schema evolution rules
  object SimpleDecider extends Schema.Decider {
    override def getSchema[D](
      dependency: VariableContext[D]
    ): Schema[D] = {
      new MapSchema[D](dependency, SimpleDecider)
    }
  }

}

class MapSchema[T](
  variableContext: VariableContext[T],
  decider: Schema.Decider
) extends Schema[T](variableContext, decider) with Slf4jLogging {

  // override the default behavior to directly cast input based on type information
  // this extra testing can slow down the conversion
  // there is also the option to reuse cached method objects based on full context path
  override def marshal(
    input: Any,
    node: NoSchema[T]
  ): T = {
    Option(input) match {
      case Some(value) =>
        val inputClassName = input.getClass.getCanonicalName
        if (currentNode.tpe.toString == inputClassName) {
          logDebug(s"input is already expected type: ${currentNode.tpe}")
          input.asInstanceOf[T]
        } else {
          logDebug(s"input ${input}[${inputClassName}] is not expected type: ${currentNode.tpe}, " +
            s"parse and perform shapeless transform")
          node.marshal(value, this)
        }
      case None => getValueForNull()
    }
  }

  override def getParser(): Parser = new Parser {

    class ParsedMap(map: Map[String, Any]) extends Parsed {

      override def removeSymbol(symbol: Symbol): Parsed =
        new ParsedMap(map - symbol.name)

      override def getSymbolValue(symbol: Symbol): Any = map.getOrElse(symbol.name, null) // scalastyle:ignore

      override def allFields(): Map[String, Any] = map
    }

    override def parse(input: Any): Parsed = {
      val map: Map[String, Any] = Option(input) match {
        case Some(value) => value match {
          case iterable: Iterable[_] =>
            iterable.map {
              case (k, v) => k.toString -> v
              case i => throw new Exception(s"$i is not expected form of Tuple2 KV pair")
            }.toMap
          case _ => throw new Exception(
            s"simple map parser expects iterable of KV pairs, input is ${value.getClass}")
        }
        case None => Map.empty
      }
      new ParsedMap(map)
    }
  }

  override def getAssembler(): Assembler = new Assembler {

    class AssembledMap extends Assembled {
      private val map = collection.mutable.Map.empty[String, Any]

      override def addSymbolValue(
        symbol: Symbol,
        value: Any
      ): Assembled = {
        map += symbol.name -> value
        this
      }

      override def value: Any = map.toMap
    }

    override def empty(): Assembled = new AssembledMap
  }
}
