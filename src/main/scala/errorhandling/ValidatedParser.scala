package errorhandling

import cats.data.NonEmptyChain.one
import cats.data._
import cats.implicits._

case object ValidatedParser extends PersonParser {

  type Validated[+T] = ValidatedNec[String, T]

  def require(cond: Boolean, msg: => String): Result[Unit] =
    if (cond) Right(Unit)
    else Left(msg)

  def required(value: Option[String]): Validated[String] = value.toValidNec("value is required")

  def integer(value: String): Validated[Int] =
    Validated.catchNonFatal(value.toInt).leftMap(_.getMessage).toValidatedNec

  def boolean(value: String): Validated[Boolean] = value match {
    case "true" => true.validNec
    case "false" => false.validNec
    case other => s"Unknown boolean `$other`".invalidNec
  }

  def person(data: Map[String, String]): Validated[Person] = {
    val name: Validated[String] =
      required(data.get("name"))
        .ensure(one("name should not be empty"))(_.nonEmpty)
    val age: Validated[Int] =
      required(data.get("age"))
        .andThen(integer)
        .ensure(one("age should be positive"))(_ > 0)
    val isMale: Validated[Boolean] =
      required(data.get("isMale"))
        .andThen(boolean)
    (name, age, isMale).mapN(Person)
  }

  override def parse(data: Map[String, String]): Result[Person] =
    person(data).leftMap(_.iterator.mkString(", ")).toEither
}
