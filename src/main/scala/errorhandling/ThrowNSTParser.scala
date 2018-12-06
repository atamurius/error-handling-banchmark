package errorhandling

import scala.util.control.{NoStackTrace, NonFatal}

case object ThrowNSTParser extends PersonParser {

  case class ValidationFailure(msg: String) extends Exception(msg) with NoStackTrace

  def required[T](value: T): T =
    if (value == null) throw ValidationFailure("Value is required")
    else value

  def require(cond: Boolean, msg: => String): Unit =
    if (!cond) throw ValidationFailure(msg)

  def string(value: String): String = required(value)

  def integer(value: String): Int = required(value).toInt

  def boolean(value: String): Boolean = value match {
    case "true" => true
    case "false" => false
    case other => throw ValidationFailure(s"Unknown boolean `$other`")
  }

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

