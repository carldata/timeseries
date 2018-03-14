package series

import java.time.Instant

import org.scalatest._


class CsvTest extends FlatSpec with Matchers {

  "Csv reader" should "read series from string" in {
    val str =
      """time,value
        |2005-01-01T12:34:15,2
        |2006-01-01T12:34:15,-4
        |2007-01-01T12:34:15,-6
        |2008-01-01T12:34:15,9""".stripMargin
    val series = Csv.fromString(str)
    series.values shouldBe Vector(2, -4, -6, 9)
  }

  it should "read custom time format" in {
    val str =
      """time,value
        |2013-09-09 10:50:00,2.805
        |2013-09-09 10:55:00,2.796
        |2013-09-09 11:00:00,2.791
        |2013-09-09 11:05:00,46.68
        |2013-09-09 11:10:00,48.03""".stripMargin
    val series = Csv.fromString(str)
    series.values shouldBe Vector(2.805, 2.796, 2.791, 46.68, 48.03)
  }

  it should "read custom time format 2" in {
    val str =
      """time,value
        |2005-01-01,2
        |2006-01-01,-4
        |2007-01-01,-6
        |2008-01-01,9""".stripMargin
    val series = Csv.fromString(str)
    series.values shouldBe Vector(2, -4, -6, 9)
  }

  "CSV Writer" should "save series to string" in {
    val now = Instant.ofEpochSecond(1000)
    val idx = Vector(now, now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3))
    val series = TimeSeries(idx, Vector(1f, 4f, 6f, 9f))
    val csv = Csv.toCsv(series)
    series shouldBe Csv.fromString(csv)
  }

}