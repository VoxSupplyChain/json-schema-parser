package json.schema.parser

import argonaut.Argonaut._
import argonaut.DecodeJson

import scalaz.Validation


trait JsonSchemaParser[N] {

  implicit def n: Numeric[N]

  implicit def dn: DecodeJson[N]

  val schemaDecoder = JsonSchemaDecoderFactory[N]

  def parse(json: String): Validation[String, SchemaDocument[N]] = json.decodeValidation[SchemaDocument[N]](schemaDecoder)
}

object JsonSchemaParser extends JsonSchemaParser[Double] {
  def n: Numeric[Double] = implicitly

  def dn: DecodeJson[Double] = implicitly

  def apply(json: String) = parse(json)
}