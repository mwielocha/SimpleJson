package com.yougov.simplejson

package object simplejson {

  object Json {

    def toJson[T](v: Any): String = ???

    def toJson(tuples: (String, Any)*): String = ???
  }
}
