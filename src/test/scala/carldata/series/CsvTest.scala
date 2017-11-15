package carldata.series

import java.time.LocalDateTime

import org.scalatest._


class CsvTest extends FlatSpec with Matchers {

  "Csv" should "read series from string" in {
    val str =
      """time,value
        |2005-01-01T12:34:15,2
        |2006-01-01T12:34:15,-4
        |2007-01-01T12:34:15,-6
        |2008-01-01T12:34:15,9""".stripMargin
    val series = Csv.fromString(str)
    series.values shouldBe Vector(2, -4, -6, 9)
  }

  it should "save series to string" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3))
    val series = TimeSeries(idx, Vector(1f, 4f, 6f, 9f))
    val expected =
      """time,value
        |2015-01-01T00:00:00,1.0
        |2015-01-01T00:01:00,4.0
        |2015-01-01T00:02:00,6.0
        |2015-01-01T00:03:00,9.0""".stripMargin
    val csv = Csv.toCsv(series)
    expected shouldBe csv
  }

}