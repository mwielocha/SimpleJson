package com.yougov.simplejson

package object simplejson {

  trait JsonWriter[-T] {
    def write(value: T): String
  }

  implicit object StringWriter extends JsonWriter[String] {
    override def write(v: String) = s""""$v""""
  }

  implicit object IntWriter extends JsonWriter[Int] {
    override def write(v: Int) = s"$v"
  }

  implicit object DoubleWriter extends JsonWriter[Double] {
    override def write(v: Double) = s"$v"
  }

  implicit def traversableWriter[T : JsonWriter]: JsonWriter[Traversable[T]] = {
    new JsonWriter[Traversable[T]] {

      override def write(v: Traversable[T]) = {

        val body = v.foldLeft(List.empty[String]) {
          case (acc, t) => acc :+ Json.toJson(t)
        }.mkString(",")

        s"[$body]"
      }
    }
  }

  case class JsonWrapper(json: String)

  implicit def any2JsonWrapper[T: JsonWriter](any: T) = {
    JsonWrapper(Json.toJson(any))
  }

  implicit object TupleListWriter extends JsonWriter[List[(String, JsonWrapper)]] {
    override def write(v: List[(String, JsonWrapper)]) = {

      import Json.toJson

      val body = v.foldLeft(List.empty[String]) {
        case (acc, (name, JsonWrapper(json))) => acc :+ s""""$name":$json"""
      }.mkString(",")

      s"{$body}"
    }
  }

  object Json {

    def define[T](f: T => List[(String, JsonWrapper)]) = {
      new JsonWriter[T] {
        override def write(v: T) = Json.toJson(f(v): _*)
      }
    }

    def toJson[T](v: T)(implicit writer: JsonWriter[T]): String = {
      writer.write(v)
    }

    def toJson(tuples: (String, JsonWrapper)*): String = {
      toJson(tuples.toList)
    }
  }
}
