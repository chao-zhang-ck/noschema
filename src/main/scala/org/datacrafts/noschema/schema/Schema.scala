package org.datacrafts.noschema.schema

import scala.util.Try

import org.datacrafts.noschema.{ActionContext, NoSchema, VariableContext}
import org.datacrafts.noschema.ActionContext.{Marshalling, Unmarshalling}
import org.datacrafts.noschema.VariableContext.{ContainerElement, LocalContext, MemberVariable}

/**
  * Schema has both marshalling and unmarshalling actions,
  * with statically cached sub-schema for each dependency variable.
  * Schema will be built once, deciding marshalling and unmarshalling operations
  * for each variable node in the tree deciding based on its full path.
  * Each node can have entirely different classes of operations for completely
  * customizable schema compatibility/evolution rules.
  * The rule can be arbitrarily complex and even query external metadata system.
  *
  * @param variableContext full context/path of the current variable node
  */
abstract class Schema[T](
  override val variableContext: VariableContext[T]
) extends ActionContext[T] with Marshalling[T] with Unmarshalling[T] {

  // cache dependency schema, so they only needs to be created once
  private val dependencySchemaMap =
    collection.mutable.Map.empty[LocalContext[_], Schema[_]]

  // initialize the entire schema tree on creation
  variableContext.localContext.node.dependencies.foreach {
    dep => getOrCreateDependencySchema(dep)
  }

  // public interface to marshal input
  final def marshal(input: Any): T = {
    this.marshal(input, variableContext.currentNode)
  }

  // public interface to unmarshal input
  final def unmarshal(input: T): Any = {
    this.unmarshal(input, variableContext.currentNode)
  }

  // override this method to determine custom Schema implementation
  protected def getSchema[D](variableContext: VariableContext[D]): Schema[D]

  private def getOrCreateDependencySchema[D](
    dependency: LocalContext[D]
  ): Schema[D] = {
    dependencySchemaMap.getOrElseUpdate(
      dependency,
      getSchema(variableContext.dependencyContext(dependency))
    ).asInstanceOf[Schema[D]]
  }

  final override def dependencyMarshalling[D](
    dependency: LocalContext[D]
  ): Marshalling[D] = getOrCreateDependencySchema(dependency)

  final override def dependencyUnmarshalling[D](
    dependency: LocalContext[D]
  ): Unmarshalling[D] = getOrCreateDependencySchema(dependency)

  lazy val schemaInfo: (String, Any) = {

    val schemaInfoKey = variableContext.localContext match {
      case MemberVariable(symbol, node) => s"${symbol.map(_.name).getOrElse("root")}: ${node}"
      case ContainerElement(node) => s"element: ${node}"
    }

    schemaInfoKey -> Map(
      "parser" -> parser,
      "assembler" -> assembler,
      "dependencies" -> dependencySchemaMap.values.map(_.schemaInfo).toMap
    )
  }

  // override this method to get custom parser implementation (marshalling)
  protected def getParser(): Option[Schema.Parser] = None

  private lazy val parser: Option[Schema.Parser] =
    if (currentNode.category == NoSchema.Category.Struct) {
    getParser()
  } else {
    None
  }

  final override def parseStruct(input: Any): Marshalling.Parsed = {
    parser
      .getOrElse(throw new Exception(s"parser for $variableContext not implemented"))
      .parseStruct(input)
  }

  // override this method to get custom assembler implementation (unmarshalling)
  protected def getAssembler(): Option[Schema.Assembler] = None

  private lazy val assembler: Option[Schema.Assembler] =
    if (currentNode.category == NoSchema.Category.Struct) {
      getAssembler()
    } else {
      None
    }

  final override def createAssembled(): Unmarshalling.Assembled = {
    assembler
      .getOrElse(throw new Exception(s"assembler for $variableContext not implemented"))
      .createAssembled()
  }
}

object Schema {

  trait Parser {
    def parseStruct(input: Any): Marshalling.Parsed
  }

  trait Assembler {
    def createAssembled(): Unmarshalling.Assembled
  }

}
