package errorhandling

import java.text.NumberFormat

import org.scalameter._
import org.scalameter.utils.Tree

class TableReporter extends Reporter[Double] {

  def report(result: CurveData[Double], persistor: Persistor) {
    // output context
    log(s"::Benchmark ${result.context.scope}::")
    val machineKeys = result.context.properties
      .filterKeys(Context.machine.properties.keySet.contains).toSeq.sortBy(_._1)
    for ((key, value) <- machineKeys) {
      log(s"$key: $value")
    }

    val paramValues = for {
      m <- result.measurements
      (p, v) <- m.params.axisData
    } yield (p.fullName, v)

    val uniqueValuesCount = paramValues
      .groupBy(_._1)
      .transform((_, vs) => vs.map(_._2).distinct.sortBy(_.toString))
      .toSeq

    uniqueValuesCount.sortBy(_._2.size) match {
      case Seq((small, smallValues), (large, largeValues)) =>
        // add a new line
        log("")
        log(s"$large\t${smallValues mkString "\t"}")
        val format = NumberFormat.getInstance()
        largeValues.foreach { y =>
          val row = result.measurements.filter(_.params(large) == y)
          val values = smallValues.map(x =>
            row.find(_.params(small) == x).map(_.value).map(format.format).getOrElse("-")
          )
          log(s"$y\t${values mkString "\t"}")
        }

      case _ =>
        log("There should be 2 params to draw table")

        // output measurements
        for (measurement <- result.measurements) {
          log(s"${measurement.params}: ${measurement.value}")
        }
    }

    // add a new line
    log("")
  }

  def report(result: Tree[CurveData[Double]], persistor: Persistor) = true

}
