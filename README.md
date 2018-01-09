[![CircleCI](https://circleci.com/gh/VoxSupplyChain/json-schema-parser.svg?style=svg)](https://circleci.com/gh/VoxSupplyChain/json-schema-parser)

JSON-Schema v4 parser and Schema validator implemented in Scala.

Features:

 * Implementation of [Schema v4][draft-zyp-json-schema-04] and [Schema Validator v4][draft-fge-json-schema-validation-00]
 * both [canonical and inline dereferencing][draft-zyp-json-schema-04#7.2.3]
 * supports scope overriding via id fields, and dereferrencing according the custom scopes.
 * expands ids to absolute uris
 * results in Schema tree
 * command line schema validator
 
Also includes implementation of:

 * JSON pointer 
 * JSON reference 

### Use
```
 resolvers += Resolver.url(
   "vox-public-ivy",
   url("http://dl.bintray.com/content/voxsupplychain/ivy-public"))(
     Resolver.ivyStylePatterns)
 libraryDependencies ++= Seq(
   "com.voxsupplychain" %% "json-schema-parser" % "0.12.0"
   )
```
 
### JSON-Schema Validator Command line
Pull the latest tag from Github, and then:

```
$ sbt assembly
$ java -jar target/scala-2.10/json-schema-parser-assembly-0.12.0.jar <schema-uri-or-path>
```
 
### Reference

 * [JSON Schema Core (Draft 4)][draft-zyp-json-schema-04]
 * [JSON Schema Validation ('Draft 4')][draft-fge-json-schema-validation-00] (actually published as I-D version 0 but extracted from JSON Schema Core from Draft 3 to Draft 4)

### Used in

 * https://github.com/VoxSupplyChain/json-schema-codegen

### Release Notes

 * 0.11.0
  * #1 support for cyclic references (@blast-hardcheese)

[draft-zyp-json-schema-04]: https://tools.ietf.org/html/draft-zyp-json-schema-04
[draft-zyp-json-schema-04#7.2.3]: https://tools.ietf.org/html/draft-zyp-json-schema-04#section-7.2.3
[draft-fge-json-schema-validation-00]: https://tools.ietf.org/html/draft-fge-json-schema-validation-00
