package org.datacrafts.noschema

import org.datacrafts.noschema.VariableContext.LocalContext

object VariableContext {

  sealed trait LocalContext[T] {
    def node: NoSchema[T]
  }

  case class MemberVariable[T](symbol: Option[Symbol], node: NoSchema[T]) extends LocalContext[T]

  case class ContainerElement[T](node: NoSchema[T]) extends LocalContext[T]

  def root[T](node: NoSchema[T]): VariableContext[T] =
    VariableContext(MemberVariable(None, node), None)
}

case class VariableContext[T](
  localContext: LocalContext[T], parentContext: Option[VariableContext[_]]) {

  def currentNode: NoSchema[T] = localContext.node

  override def toString: String =
    s"${parentContext.map(pc => s"${pc}").getOrElse("")}.${localContext}"

  def dependencyContext[D](context: LocalContext[D]): VariableContext[D] =
    VariableContext(localContext = context, parentContext = Some(this))
}
