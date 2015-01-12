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

  val additionalPropertiesMember = "_additional"

  def generateFile(scope: URI, fileName: String, outputDir: Path)(content: String => Validation[String, String]): Validation[String, Seq[Path]] = {

    val packageName = genPackageName(scope)

    try {

      val packageDir: String = packageName.replaceAll("\\.", File.separator)
      val modelFile = packageDir + File.separator + fileName

      // create package structure
      Files.createDirectories(outputDir.resolve(packageDir))
      content(packageName) map {
        fileContent =>
          Seq(
            Files.write(outputDir.resolve(modelFile), fileContent.getBytes(StandardCharsets.UTF_8))
          )
      }

    } catch {
      case NonFatal(e) => e.getMessage.failure
    }

  }

  def generateCodec(ts: Iterable[ScalaType], scope: URI, outputDir: Path): Validation[String, Seq[Path]] = {
    generateFile(scope, "Codecs.scala", outputDir) {
      packageName =>

        val codecs = ts.map {
          case t: ScalaClass => genCodec(t)
          case _ => ""
        }.mkString("\n")

        s"""
         |package $packageName
         |
         |import argonaut._, Argonaut._
         |
         |object Codecs {
         |$codecs
         |}
        """.stripMargin.success

    }

  }

  def generateModel(ts: Iterable[ScalaType], scope: URI, outputDir: Path): Validation[String, Seq[Path]] = {
    generateFile(scope, "model.scala", outputDir) {
      packageName =>

        ts.map {
          case t: ScalaClass => genType(t)
          case _ => ""
        }.mkString(
            s"package $packageName\n\n",
            "\n\n", ""
          ).success

    }
  }

  def genCodec(c: ScalaClass): String = {
    val propNames = c.properties.map(p => '"' + p.name + '"').mkString(", ")
    val className = c.identifier
    c.additionalNested.fold(
      s"""
       |implicit def ${className}Codec=casecodec${c.properties.length}("$className.apply", "$className.unapply")($propNames)
     """.stripMargin
    )(
        additionalType =>
          s"""
           |implicit def ${className}Codec=CodecJson(MapEncodeJson[$className], MapDecodeJson[$className])
         """.stripMargin
      )
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
          val propType = genPropertyType(tn)
          s"$additionalPropertiesMember:Map[String, $propType]"
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

  def gen[N: Numeric, T: JsonSource](jsonParser: JsonSchemaParser[N], source: T)(codeGenTarget: Path) = {
    for {
      schema <- jsonParser.parse(source).validation.pp
      models <- ScalaModelGenerator(schema).pp
      modelFiles <- generateModel(models, schema.scope, codeGenTarget).pp
      codecFiles <- generateCodec(models, schema.scope, codeGenTarget).pp
    } yield modelFiles ++ codecFiles

  }

  def main(args: Array[String]) {

    val source: File = new File(args(0))

    val genRoot: Path = FileSystems.getDefault.getPath("/tmp")
    val generator = if (true) gen(JsonSchemaParser, source)(_) else gen(new JsonSchemaParser[Float], source)(_)

    val result = generator(genRoot)
    println(result)

    if (result.isFailure) System.exit(1) else System.exit(0)

  }
}