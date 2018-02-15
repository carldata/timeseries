package carldata.series

import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField


object Csv {

  private val defaultFormatter = new DateTimeFormatterBuilder()
    .parseCaseInsensitive
    .appendValue(ChronoField.YEAR)
    .appendLiteral('-')
    .appendValue(ChronoField.MONTH_OF_YEAR)
    .appendLiteral('-')
    .appendValue(ChronoField.DAY_OF_MONTH)
    .optionalStart.appendLiteral(' ').optionalEnd
    .optionalStart.appendLiteral('T').optionalEnd
    .optionalStart
    .appendValue(ChronoField.HOUR_OF_DAY)
    .appendLiteral(':')
    .appendValue(ChronoField.MINUTE_OF_HOUR)
    .optionalStart.appendLiteral(':').appendValue(ChronoField.SECOND_OF_MINUTE).optionalEnd
    .optionalStart.appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true).optionalEnd
    .optionalStart.appendLiteral('Z').optionalEnd
    .optionalEnd
    .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
    .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
    .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
    .parseDefaulting(ChronoField.NANO_OF_SECOND, 0)
    .toFormatter


  /** Reader for CSV string */
  def fromString(str: String, dateFormatter: DateTimeFormatter = defaultFormatter): TimeSeries[Double] = {
    val data = str.split("\n").tail.map{ line =>
      val tokens = line.split(",")
      (LocalDateTime.parse(tokens(0), dateFormatter).toInstant(ZoneOffset.UTC), tokens(1).toDouble)
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
