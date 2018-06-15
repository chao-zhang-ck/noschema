package org.datacrafts.noschema.context

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{ActionContext, NoSchema, VariableContext}
import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}
import org.datacrafts.noschema.ActionContext.Marshalling.{Parsed, Parser}
import org.datacrafts.noschema.ActionContext.Unmarshalling.{Assembled, Assembler}
import org.datacrafts.noschema.VariableContext.LocalContext

/**
  * Marshalling from and unmarshalling to Map[String, Any] without much customization
  */
object SimpleMapContext {

  def noSchema[T: NoSchema]: SimpleMapContext[T] =
    new SimpleMapContext(VariableContext.root(implicitly[NoSchema[T]]))

}

class SimpleMapContext[T](override val variableContext: VariableContext[T])
  extends ActionContext[T] with Marshalling[T] with Unmarshalling[T] with Slf4jLogging {

  // override the default behavior to directly cast input based on type information
  // this extra testing can slow down the conversion
  // there is also the option to reuse cached method objects based on full context path
  override def marshal(input: Any, node: NoSchema[T]): T = {
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

  def marshal(input: Any): T = {
    this.marshal(input, variableContext.currentNode)
  }

  def unmarshal(input: T): Any = {
    this.unmarshal(input, variableContext.currentNode)
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

  override def dependencyMarshalling[D](localContext: LocalContext[D]): Marshalling[D] = {
    new SimpleMapContext(
      variableContext.dependencyContext(localContext)
    )
  }

  override def getAssembler(): Assembler = new Assembler {

    class AssembledMap extends Assembled {
      private val map = collection.mutable.Map.empty[String, Any]

      override def addSymbolValue(symbol: Symbol, value: Any): Assembled = {
        map += symbol.name -> value
        this
      }

      override def value: Any = map.toMap
    }

    override def empty(): Assembled = new AssembledMap
  }

  override def dependencyUnmarshalling[D](localContext: LocalContext[D]): Unmarshalling[D] = {
    new SimpleMapContext(variableContext.dependencyContext(localContext))
  }
}
