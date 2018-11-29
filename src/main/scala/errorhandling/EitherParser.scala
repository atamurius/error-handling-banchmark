package errorhandling

import cats.syntax.either._

import scala.util.Try

case object EitherParser extends PersonParser {

  def require(cond: Boolean, msg: => String): Result[Unit] =
    if (cond) Right(Unit)
    else Left(msg)

  def required(value: Option[String]): Result[String] =
    value.fold("value is required".asLeft[String])(_.asRight)

  def integer(value: String): Result[Int] = Try(value.toInt).toEither.left.map(_.getMessage)

  def boolean(value: String): Result[Boolean] = value match {
    case "true" => true.asRight
    case "false" => false.asRight
    case other => s"Unknown boolean `$other`".asLeft
  }

  def person(data: Map[String, String]): Result[Person] = for {
    name    <- required(data.get("name"))
    age     <- required(data.get("age")) flatMap integer
    isMale  <- required(data.get("isMale")) flatMap boolean
    _       <- require(name.nonEmpty, "name should not be empty")
    _       <- require(age > 0, "age should be positive")
  } yield Person(name, age, isMale)

  override def parse(data: Map[String, String]): Result[Person] = person(data)
}
