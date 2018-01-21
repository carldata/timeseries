package carldata.series

import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

object Csv {

  /** Reader for CSV string */
  def fromString(str: String): TimeSeries[Double] = {
    val data = str.split("\n").tail.map{ line =>
      val tokens = line.split(",")
      (LocalDateTime.parse(tokens(0)).toInstant(ZoneOffset.UTC), tokens(1).toDouble)
    }
    new TimeSeries(data)
  }

  /** Write TimeSeries to CSV string */
  def toCsv[A](xs: TimeSeries[A]): String = {
    val header = "time,value"
    val lines = xs.dataPoints.map(x => LocalDateTime.ofInstant(x._1, ZoneOffset.UTC)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "," + x._2.toString)
    header + "\n" + lines.mkString("\n")
  }
}
