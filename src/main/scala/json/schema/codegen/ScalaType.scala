package json.schema.codegen

import scalaz.Scalaz._

case class ScalaTypeProperty(name: String, required: Boolean, isa: ScalaType) {
  def propertyType = required ? isa.identifier | s"Option[${isa.identifier}]"
}

sealed trait ScalaType {
  val identifier: String
}

sealed case class ScalaScalar(identifier: String) extends ScalaType

sealed case class ScalaArray(unique: Boolean, nested: ScalaType) extends ScalaType {
  val identifier = if (unique) s"Set[${nested.identifier}]" else s"List[${nested.identifier}]"
}

sealed case class ScalaClass(identifier: String, properties: List[ScalaTypeProperty]) extends ScalaType




