package json.schema.codegen

import scalaz.Scalaz._

case class ScalaTypeProperty(name: String, required: Boolean, isa: ScalaType) {
  override def toString = required ? isa.identifier | s"Option[${isa.identifier}]"
}

sealed trait ScalaType {
  val identifier: String
  override def toString = identifier
}

sealed case class ScalaSimple(identifier: String) extends ScalaType

sealed case class ScalaArray(unique: Boolean, nested: ScalaType) extends ScalaType {
  val identifier  = nested.identifier
  override def toString = if (unique) s"Set[${nested.identifier}]" else s"List[${nested.identifier}]"
}

sealed case class ScalaClass(identifier: String, properties: List[ScalaTypeProperty], additionalNested: Option[ScalaType]) extends ScalaType

sealed case class ScalaEnum(identifier: String, nested: ScalaType, enums: Set[_]) extends ScalaType




