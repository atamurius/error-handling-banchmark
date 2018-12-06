package errorhandling

import org.scalameter._
import org.scalameter.api.{Bench, Gen}
import org.scalameter.picklers.Implicits._

import scala.util.Random

object Performance extends Bench.LocalTime {

  def generate(failureChance: Double): Map[String, String] =
    Map(
      "name" -> (Random.nextDouble() match {
        case x if x < failureChance / 2 => None
        case x if x < failureChance => Some("")
        case _ => Some("correct name")
      }),
      "age" -> (Random.nextDouble() match {
        case x if x < failureChance * 0.5 => None
//        case x if x < failureChance * 0.7 => Some(s"wrong")
        case x if x < failureChance => Some(s"-${Random.nextInt(100)}")
        case _ => Some(s"${Random.nextInt(80) + 1}")
      }),
      "isMale" -> (Random.nextDouble() match {
        case x if x < failureChance * 0.5 => None
        case x if x < failureChance => Some("wrong")
        case _ => Some(Random.nextBoolean().toString)
      })
    ).collect {
      case (key, Some(value)) => key -> value
    }

  override def reporter: Reporter[Double] = new TableReporter

  case class DataSet(failurePercent: Double, n: Int) {
    final val data = Vector.fill(n)(generate(failurePercent / 100.0))
    final val failures = data.count(d => EitherParser.parse(d).isLeft).toDouble / n
    override def toString: String = f"$failures%.3f".replace('.', ',')
  }

  private val fullDataSet =
    ((0.0 until 2.0 by 0.2) ++
      (2.0 until 10 by 1) ++
      (10.0 until 30 by 5) ++
      (30.0 to 100 by 10)
    ).map(DataSet(_, 10000))

  private val smallDataSet =
    ((0.0 until 2.0 by 0.1) ++
      (2.0 until 10 by 0.5)
    ).map(DataSet(_, 15000))

  private val dataSets = smallDataSet.map(d => d.toString -> d).toMap

  private val parserGen = Gen.enumeration("Type")(parsers.keys.toSeq: _*).map(parsers)

  private val dataGen = Gen.enumeration("Failure")(dataSets.keys.toSeq: _*).map(dataSets).cached

  performance of "PersonParser" in {
    performance of "parse" in {
      using(Gen.crossProduct(parserGen, dataGen)) in { case (parser, dataset) =>
        dataset.data.map(parser.parse)
      }
    }
  }
}
