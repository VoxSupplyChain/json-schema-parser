package json.schema.validator

import java.io.File
import java.net.URI

import json.schema.parser.JsonSchemaParser

import scala.util.control.NonFatal
import scalaz.{Failure, Success}

object JsonSchemaValidator {

  def main(args: Array[String]): Unit = {

    if (args.length==1) {
      val schemaUri = args(0)

      val result = try {
        JsonSchemaParser.parse(new URI(schemaUri))
      } catch {
        case NonFatal(_) => JsonSchemaParser.parse(new File(schemaUri))
      }

      result.validation match {
        case Failure(f) => System.err.println(s"invalid schema at $schemaUri: $f")
        case Success(schema) => System.out.println(s"valid schema at $schemaUri: $schema")
      }

    } else {
      System.err.println(s"Usage: ${getClass.getCanonicalName} <schema-uri-or-path> ")
      System.exit(1)
    }

  }

}
