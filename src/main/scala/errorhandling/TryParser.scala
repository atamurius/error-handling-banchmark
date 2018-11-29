package errorhandling

import scala.util.{Failure, Success, Try}

case object TryParser extends PersonParser {

  def require(cond: Boolean, msg: => String): Try[Unit] =
    if (cond) Success(Unit)
    else Failure(new IllegalArgumentException(msg))

  def required(value: Option[String]): Try[String] = value match {
    case None => Failure(new IllegalArgumentException("value is required"))
    case Some(x) => Success(x)
  }

  def integer(value: String): Try[Int] = Try(value.toInt)

  def boolean(value: String): Try[Boolean] = Try(value.toBoolean)

  def person(data: Map[String, String]): Try[Person] = for {
    name    <- required(data.get("name"))
    age     <- required(data.get("age")) flatMap integer
    isMale  <- required(data.get("isMale")) flatMap boolean
    _       <- require(name.nonEmpty, "name should not be empty")
    _       <- require(age > 0, "age should be positive")
  } yield Person(name, age, isMale)

  override def parse(data: Map[String, String]): Result[Person] =
    person(data).toEither.left.map(_.getMessage)
}