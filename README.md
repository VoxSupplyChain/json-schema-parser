# json-schema-parser

[![CircleCI](https://circleci.com/gh/VoxSupplyChain/json-schema-parser.svg?style=svg)](https://circleci.com/gh/VoxSupplyChain/json-schema-parser)

JSON-Schema v4 parser and Schema validator implemented in Scala.

Features:

 * Implementation of [Schema v4](http://json-schema.org/latest/json-schema-core.html) and [Schema Validator v4](http://json-schema.org/latest/json-schema-validation.html)
 * both [canonical and inline dereferencing](http://json-schema.org/latest/json-schema-core.html#anchor30)
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
   "com.voxsupplychain" %% "json-schema-parser" % "0.11.0"
   )
```
 
### JSON-Schema Validator Command line
Pull the latest tag from Github, and then:

```
$ sbt assembly
$ java -jar target/scala-2.10/json-schema-parser-assembly-0.11.0.jar <schema-uri-or-path>
```
 
### Reference

 * http://json-schema.org/latest/json-schema-core.html
 * http://json-schema.org/latest/json-schema-validation.html
 
### Used in

 * https://github.com/VoxSupplyChain/json-schema-codegen

### Release Notes

 * 0.11.0
  * #1 support for cyclic references (@blast-hardcheese)
