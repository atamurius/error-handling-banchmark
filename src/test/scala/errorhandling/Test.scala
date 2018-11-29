package errorhandling

import org.scalatest.{Matchers, WordSpec}

class Test extends WordSpec with Matchers {

  "Person parser" should {

    "parse all exampes with same results" in {
      val total = 10000
      var correct = 0
      for (_ <- 1 to total) {
        val data = Performance.generate(0.2)
        val results = Seq(
          ThrowParser.parse(data),
          TryParser.parse(data),
          EitherParser.parse(data),
          ValidatedParser.parse(data)
        )
        // either all None or all same Some(person):
        if (results.map(_.toOption).distinct.size > 1) fail(
          s"""
             |Failed on $data:
             |${results.mkString("\n")}
         """.stripMargin)

        if (results.head.isRight) correct += 1
      }
      println(s"Correct: ${correct * 100 / total}%")
    }
  }
}
