package com.yougov.simplejson

import org.scalatest._
import simplejson._

class SimpleJsonSpec extends FlatSpec with Matchers {

  case class Person(name: String, age: Int)

  implicit val personWriter = Json.define[Person] {
    v =>
      List(
        "name" -> v.name,
        "age" -> v.age
      )
  }

  case class Employee(person: Person, salary: Double)

  implicit val employeeWriter = Json.define[Employee] {
    v =>
      List(
        "person" -> v.person,
        "salary" -> v.salary
      )
  }

  "SimpleJson" should "serialize int to json" in {

    import simplejson._

    val a: Int = 42

    Json.toJson(a) shouldBe "42"

  }

  it should "serialize string to json" in {

    val a: String = "John"

    Json.toJson(a) shouldBe "\"John\""

  }

  it should "serialize a list of tuples to json" in {

    import simplejson._

    val list: List[(String, JsonWrapper)] = List(
      "name" -> "John",
      "age" -> 42
    )

    Json.toJson(list) shouldBe """{"name":"John","age":42}"""

  }

  it should "serialize a case class to json" in {
    
    case class Person(name: String, age: Int)

    implicit object PersonWriter extends JsonWriter[Person] {
      override def write(v: Person) = {
        Json.toJson(
          "name" -> v.name,
          "age" -> v.age
        )
      }
    }

    val a = Person("John", 42)

    Json.toJson(a) shouldBe """{"name":"John","age":42}"""

  }

  it should "serialize a nested case class to json" in {

    val a = Employee(Person("John", 42), 100)

    Json.toJson(a) shouldBe """{"person":{"name":"John","age":42},"salary":100.0}"""

  }

  it should "serialize an array of ints" in {

    val list: List[Int] = List(1, 2, 3)

    Json.toJson(list) shouldBe """[1,2,3]"""

  }

  it should "serialize an array of objects" in {

    case class User(name: String)

    implicit val userWriter = Json.define[User] {
      u =>
        List(
          "name" -> u.name
        )
    }

    val list = List(
      User("John"),
      User("Jim")
    )

    Json.toJson(list) shouldBe """[{"name":"John"},{"name":"Jim"}]"""
  }

  it should "serialize set of ints" in {

    val set = Set(1, 2, 3)

    Json.toJson(set) shouldBe """[1,2,3]"""

  }

  it should "serialize a list of nested objects" in {

    val p1 = Person("John", 42)
    val p2 = Person("Jim", 26)

    val e1 = Employee(p1, 200.6)
    val e2 = Employee(p2, 123.5)

    val list = List(e1, e2)

    Json.toJson(list) shouldBe """[{"person":{"name":"John","age":42},"salary":200.6},{"person":{"name":"Jim","age":26},"salary":123.5}]"""
  }
}
