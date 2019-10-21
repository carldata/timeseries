package carldata.series

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{LocalDateTime, ZoneOffset}


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
  def fromString(str: String, dateFormatter: DateTimeFormatter = defaultFormatter): Seq[TimeSeries[Double]] = {
    val data = str.split("\n").tail.map { line =>
      val tokens = line.replace(",,", ",NaN,").trim
      val tokens2 = (if (tokens.last == ',') tokens + "NaN" else tokens)
        .split(",") //handle missing values by marking them as Nan
      (LocalDateTime.parse(tokens2(0), dateFormatter).toInstant(ZoneOffset.UTC), tokens2.tail.map(_.toDouble).toVector)
    }.toVector

    data.unzip._2
      .transpose
      .map(vs => TimeSeries(data.unzip._1, vs).filter(!_._2.isNaN))
  }

  /** Write TimeSeries to CSV string */
  def toCsv[A](xs: TimeSeries[A]): String = {
    val header = "time,value"
    val lines = xs.dataPoints.map(x => LocalDateTime.ofInstant(x._1, ZoneOffset.UTC)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "," + x._2.toString)
    header + "\n" + lines.mkString("\n")
  }

}
