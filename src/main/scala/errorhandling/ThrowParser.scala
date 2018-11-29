package errorhandling

import java.util.Objects

import scala.util.control.NonFatal

case object ThrowParser extends PersonParser {

  def string(value: String): String = Objects.requireNonNull(value)

  def integer(value: String): Int = value.toInt

  def boolean(value: String): Boolean = value.toBoolean

  def person(data: Map[String, String]): Person = {
    val name = string(data.getOrElse("name", null))
    val age = integer(data.getOrElse("age", null))
    val isMale = boolean(data.getOrElse("isMale", null))
    require(name.nonEmpty, "name should not be empty")
    require(age > 0, "age should be positive")
    Person(name, age, isMale)
  }

  override def parse(data: Map[String, String]): Result[Person] =
    try Right(person(data))
    catch {
      case NonFatal(e) => Left(e.getMessage)
    }
}

