package object errorhandling {

  type Result[T] = Either[String, T]

  case class Person(name: String, age: Int, isMale: Boolean)

  trait PersonParser {
    def parse(data: Map[String, String]): Result[Person]
  }

}
