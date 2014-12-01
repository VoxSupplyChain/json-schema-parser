package json.schema.parser

import java.net.URI

trait URIResolver[N] {
  this: JsonSchemaDecoderFactory[N] =>
  def resolve(parent: URI, sub: URI) = {
    val resolved = parent.resolve(sub)
    if (resolved.getFragment == null || resolved.getFragment.isEmpty) resolved.resolve("#") else resolved
  }

  def dereference(uri: URI): Schema = {
    null
  }
}
