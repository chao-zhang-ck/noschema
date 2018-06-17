package org.datacrafts.noschema.schema

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{NoSchema, VariableContext}
import org.datacrafts.noschema.ActionContext.Marshalling.Parsed
import org.datacrafts.noschema.ActionContext.Unmarshalling.Assembled

/**
  * Marshalling from and unmarshalling to Map[String, Any]
  */
object MapSchema {

  def schema[T: NoSchema]: Schema[T] =
    new MapSchema[T](VariableContext.root(implicitly[NoSchema[T]]))

}

class MapSchema[T](
  variableContext: VariableContext[T]
) extends Schema[T](variableContext) with Slf4jLogging {

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

  class ParsedMap(map: Map[String, Any]) extends Parsed {

    override def removeSymbol(symbol: Symbol): Parsed =
      new ParsedMap(map - symbol.name)

    override def getSymbolValue(symbol: Symbol): Any = map.getOrElse(symbol.name, null) // scalastyle:ignore

    override def allFields(): Map[String, Any] = map
  }

  override protected def getParser(): Option[Schema.Parser] = Some(
    new Schema.Parser {
      override def toString: String = "MapParser"

      override def parseStruct(input: Any): Parsed = {
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
  )

  class AssembledMap extends Assembled {
    private val map = collection.mutable.Map.empty[String, Any]

    override def addSymbolValue(
      symbol: Symbol,
      value: Any
    ): Assembled = {
      map += symbol.name -> value
      this
    }

    override def assembledValue: Any = map.toMap
  }

  override protected def getAssembler(): Option[Schema.Assembler] = Some(
    new Schema.Assembler {

      override def toString: String = "MapAssembler"

      override def createAssembled(): Assembled = new AssembledMap
    }
  )

  override def getSchema[D](variableContext: VariableContext[D]): Schema[D] =
    new MapSchema(variableContext)
}
