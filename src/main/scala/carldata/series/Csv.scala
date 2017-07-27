package carldata.series

import java.time.LocalDateTime

object Csv {

  /** Reader for CSV string */
  def fromString(str: String): TimeSeries[Double] = {
    val data = str.split("\n").tail.map{ line =>
      val tokens = line.split(",")
      (LocalDateTime.parse(tokens(0)), tokens(1).toDouble)
    }
    new TimeSeries(data)
  }
}
