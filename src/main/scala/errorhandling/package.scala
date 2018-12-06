package object errorhandling {

  type Result[T] = Either[String, T]

  case class Person(name: String, age: Int, isMale: Boolean)

  trait PersonParser {
    def parse(data: Map[String, String]): Result[Person]
  }

  val parsers: Map[String, PersonParser] = Seq(
    ThrowParser,
    ThrowNSTParser,
    TryParser,
    EitherParser,
    ValidatedParser
  ).map(p => p.productPrefix.stripSuffix("Parser") -> p).toMap

}
