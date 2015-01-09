package json.schema.codegen

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files, Path}

import json.schema.parser.JsonSchemaParser
import json.source.JsonSource

import scala.util.control.NonFatal
import scalaz.Scalaz._
import scalaz.Validation

trait CodeGen extends Naming {

  def generate(ts: Iterable[ScalaType], scope: URI)(outputDir: Path): Validation[String, Seq[Path]] = {

    val packageName = genPackageName(scope)

    try {
      val models = ts.map {
        case t: ScalaClass => genType(t)
        case _ => ""
      }.mkString(
          s"package $packageName\n\n",
          "\n\n", ""
        )

      val packageDir: String = packageName.replaceAll("\\.", File.separator)
      val modelFile = packageDir + File.separator + "model.scala"

      // create package structure
      Files.createDirectories(outputDir.resolve(packageDir))
      // model file
      Seq(
        Files.write(outputDir.resolve(modelFile), models.getBytes(StandardCharsets.UTF_8))
      ).success

    } catch {
      case NonFatal(e) => e.getMessage.failure
    }

  }

  def genPackageName(scope: URI) = {
    val simpleScope = scope.getFragment.some orElse scope.getPath.some getOrElse scope.getHost
    simpleScope.map(c => c.isLetterOrDigit ? c | '.').replaceAll("\\.+$", "").replaceAll("^\\.+", "")
  }

  def genPropertyType(t: ScalaType): String = {
    t match {
      case a: ScalaArray => if (a.unique) s"Set[${a.nested.identifier}]" else s"List[${a.nested.identifier}]"
      case a: ScalaType => a.identifier
    }
  }

  def genPropertyType(p: ScalaTypeProperty): String = {
    val t = genPropertyType(p.isa)
    p.required ? t | s"Option[$t]"
  }

  def genType(clazz: ScalaType): String = clazz match {
    case t: ScalaClass =>
      val properties = t.properties.map {
        p =>
          val propType = genPropertyType(p)
          val member = underscoreToCamel(identifier(p.name))
          s"$member:$propType"
      }
      val extra = t.additionalNested.map {
        tn =>
          val propType= genPropertyType(tn)
          s"extras:Map[String, $propType]"
      }
      val members = (properties ++ extra.toList).mkString(", ")
      s"""case class ${t.identifier}($members)""".stripMargin

    // enum of number are not support
    case t: ScalaEnum if t.nested.identifier == "Double" => ""
    case t: ScalaEnum =>
      val valueDeclarations = t.enums.map {
        case v: String =>
          val valueId = identifier(v)
          s"""val $valueId = Value("$v")"""
        case v: Int =>
          val valueId = s"v${v.toInt}"
          s"val $valueId = Value(${v.toInt})"
        case v: Double =>
          val valueId = s"v${v.toInt}"
          s"val $valueId = Value(${v.toInt})"
        case _ => ""
      }.filter(_ != "").mkString("\n")
      s"""object ${t.identifier} extends Enumeration {
         |$valueDeclarations
         |}""".stripMargin

    case _ => ""

  }

}


object CodeGenerator extends CodeGen {

  private implicit class Printable[T](v: T) {
    def pp: T = {
      println(v)
      v
    }
  }

  def gen[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N], source: T) = {
    for {
      schema <- jsonParser.parse(source).validation.pp
      models <- ScalaModelGenerator(schema).pp
      files <- generate(models, schema.scope)(FileSystems.getDefault.getPath("/tmp")).pp
    } yield files

  }

  def main(args: Array[String]) {

    val source: File = new File(args(0))

    val result = if (true) gen(JsonSchemaParser, source) else gen(new JsonSchemaParser[Float], source)

    println(result)

    if (result.isFailure) System.exit(1) else System.exit(0)

  }
}