package json

import java.net.URI

import argonaut.Json

import scalaz.\/

package object reference {

  type Loader = URI => String \/ (Json, URI)


}
