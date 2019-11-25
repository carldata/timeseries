package carldata.series

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField
import java.time.{Instant, LocalDateTime, ZoneOffset}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


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

  /** Write Sequence of TimeSeries to CSV string */
  def toComplexCsv[A](tss: Seq[TimeSeries[A]]): String = {
    @tailrec def join(xs: Vector[(Instant, String)],
                      ys: Vector[(Instant, String)], res: Vector[(Instant, String)]): Vector[(Instant, String)] = {
      if (xs.isEmpty) {
        res ++ ys.map(y => (y._1, List("", y._2).mkString(",")))
      }
      else if (ys.isEmpty) {
        xs.map(x => (x._1, List(x._2, "").mkString(","))) ++ res
      }
      else {
        val x = xs.head
        val y = ys.head
        if (x._1 == y._1) {
          join(xs.tail, ys.tail, res :+ (x._1, x._2 + "," + y._2))
        } else if (x._1.isBefore(y._1)) {
          join(xs.tail, ys, res :+ (x._1, x._2 + ","))
        } else {
          join(xs, ys.tail, res :+ (x._1, "," + y._2))
        }
      }
    }

    def stringDataPoints(ts: TimeSeries[A]): Vector[(Instant, String)] = {
      ts.dataPoints.map(x => (x._1, x._2.toString))
    }

    if (tss.nonEmpty) {
      val header: String = "time," + (for (i <- 1 to tss.size) yield s"value $i").mkString(",")
      val body: String = tss.tail.foldLeft(stringDataPoints(tss.head)) {
        (l, r) =>
          join(l, stringDataPoints(r), Vector())
      }
        .map(x => x._1.toString + "," + x._2)
        .mkString(System.lineSeparator())

      header + System.lineSeparator() + body
    }
    else "time,value"
  }
}
