package org.datacrafts.noschema.schema

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
  * @param decider         arbitrary rules to determine schema operation implementation
  */
class Schema[T](
  override val variableContext: VariableContext[T],
  decider: Schema.Decider
) extends ActionContext[T] with Marshalling[T] with Unmarshalling[T] {

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

  private lazy val dependencySchemaMap =
    collection.mutable.Map.empty[LocalContext[_], Schema[_]]

  private def getOrCreateDependencySchema[D](
    dependency: LocalContext[D]
  ): Schema[D] = {
    dependencySchemaMap.getOrElseUpdate(
      dependency,
      decider.getSchema(variableContext.dependencyContext(dependency))
    ).asInstanceOf[Schema[D]]
  }

  final override def dependencyMarshalling[D](
    dependency: LocalContext[D]
  ): Marshalling[D] = getOrCreateDependencySchema(dependency)

  final override def dependencyUnmarshalling[D](
    dependency: LocalContext[D]
  ): Unmarshalling[D] = getOrCreateDependencySchema(dependency)

  lazy val schemaInfo: String = {

    variableContext.localContext match {
      case MemberVariable(symbol, node) => s"${symbol.map(_.name).getOrElse("root")}: ${node}"
      case ContainerElement(node) => s"element: ${node}"
    }
  }

  lazy val dependencySchemaMapInfo: Map[String, Any] = {
    dependencySchemaMap.values.map{
      dependencySchema => dependencySchema.schemaInfo -> dependencySchema.dependencySchemaMapInfo
    }.toMap
  }

}

object Schema {

  trait Decider {
    def getSchema[D](variableContext: VariableContext[D]): Schema[D]
  }

}
