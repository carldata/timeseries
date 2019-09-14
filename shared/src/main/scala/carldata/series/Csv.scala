package carldata.series

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{LocalDateTime, ZoneOffset}

import scala.collection.mutable.ArrayBuffer


object Csv {

  private val newLineDelimeter = "\n"
  private val tokensDelimeter = ","

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
    val lines = str.split(newLineDelimeter)
    require(lines.nonEmpty)
    val header = lines.head
    val columnsCount = header.split(tokensDelimeter) match {
      case Array("time", "value") => 2
      case arr if arr.size > 2 && arr(0) == "time" &&
        arr.zipWithIndex.tail.forall { case (p, i) => p == s"value$i" } => arr.size
      case _ => throw new IllegalArgumentException("Wrong input string format")
    }
    val valueColumnsCount = columnsCount - 1

    val data = Vector.fill(valueColumnsCount)(ArrayBuffer.empty[(Long, Double)])
    for (line <- lines.tail) {
      val tokens = line.split(tokensDelimeter)
      require(tokens.size == columnsCount)
      val instant = LocalDateTime.parse(tokens(0), dateFormatter).toEpochSecond(ZoneOffset.UTC)

      for (idx <- 1 until tokens.size) {
        val token = tokens(idx)
        if (token.nonEmpty) {
          data(idx - 1) += ((instant, token.toDouble))
        }
      }
    }

    data.map(TimeSeries.fromTimestamps(_))
  }

  /** Write TimeSeries to CSV string */
  def toCsv[A](xs: TimeSeries[A]): String = {
    val header = "time,value"
    val lines = xs.dataPoints.map(x => LocalDateTime.ofInstant(x._1, ZoneOffset.UTC)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "," + x._2.toString)
    header + "\n" + lines.mkString("\n")
  }

  /** Write Sequence of TimeSeries to CSV string */
  def toComplexCsv[A](tss: Seq[TimeSeries[A]])(implicit num: Numeric[A]): String = {
    require(tss.nonEmpty)
    val header =
      tss.size match {
        case 1 => "time,value"
        case l =>
          val valueCols = Vector.tabulate(l) { idx => "value" + (idx + 1) }
          "time" + valueCols.mkString(",", ",", "")
      }
    val data = tss.foldLeft(TimeSeries.empty[Vector[A]]) { case (acc, ts) =>
      val joined = acc.joinOuter(ts, Vector.fill(acc.length)(num.zero), num.zero)
      joined.mapValues { case (vs, v) => vs :+ v }
    }
    val lines = data.dataPoints.map { case (instant, vec) =>
      val time = LocalDateTime
        .ofInstant(instant, ZoneOffset.UTC)
        .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      val values = vec.mkString(tokensDelimeter)
      time + tokensDelimeter + values
    }
    header + newLineDelimeter + lines.mkString(newLineDelimeter)
  }

}
