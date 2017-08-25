package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import org.scalatest._


class TimeSeriesTest extends FlatSpec with Matchers {

  "TimeSeries" should "have length equal to its index" in {
    val now = LocalDateTime.now()
    val series = new TimeSeries(Seq((now,1), (now.plusMinutes(1), 2), (now.plusMinutes(2), 3)))
    series.length shouldBe 3
  }

  it should "be build from the rows" in {
    val now = LocalDateTime.now()
    val series = TimeSeries(Vector(now, now.plusMinutes(1), now.plusMinutes(2)), Vector(1, 2, 3, 4, 5.6))
    series.length shouldBe 3
  }

  it should "be build from timestamps" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.length shouldBe 3
  }

  it should "access element at given index" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(2) shouldBe 5.6
  }

  it should "return 0 for element outside of the index" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(20) shouldBe 0
  }

  it should "return first element of a Series" in {
    val now = LocalDateTime.now()
    val series = TimeSeries(Vector(now, now.plusMinutes(1), now.plusMinutes(2)), Vector(1, 2, 3, 4, 5.6))
    series.head shouldBe Some((now, 1))
  }

  it should "return None as first element for empty Series" in {
    val series = TimeSeries.empty[Int]
    series.head shouldBe None
  }

  it should "return last element of a Series" in {
    val now = LocalDateTime.now()
    val series = TimeSeries(Vector(now, now.plusMinutes(1), now.plusMinutes(2)), Vector(1, 2, 3, 4, 5.6))
    series.last shouldBe Some((now.plusMinutes(2), 3))
  }

  it should "return None as last element for empty Series" in {
    val series = TimeSeries.empty[Int]
    series.last shouldBe None
  }

  it should "return minimum value" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.min shouldBe -3.4
  }

  it should "return maximum value" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.max shouldBe 5
  }

  it should "sum its elements" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.sum shouldBe 3
  }

  it should "map over its values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.map(x => x._2 + 2).max shouldBe 7.6
  }

  it should "mapValues over its values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.mapValues(_ + 2).max shouldBe 7.6
  }

  it should "fold values" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6)))
    series.values.sum shouldBe 4
  }

  it should "filter its values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.filter(x => x._2 > 0).length shouldBe 2
  }

  it should "return subseries" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 6)))
    val start = LocalDateTime.ofEpochSecond(2, 0, ZoneOffset.UTC)
    val end = LocalDateTime.ofEpochSecond(6, 0, ZoneOffset.UTC)
    series.slice(start, end).length shouldBe 4
  }

  it should "differentiate" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -6), (3, -2), (4, 14)))
    series.differentiate.values shouldBe expected.values
  }

  it should "integrate values" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -2), (3, -10), (4, 2)))
    series.integrate.values shouldBe expected.values
  }

  it should "integrate index" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -2), (3, -10), (4, 2)))
    series.integrate.index shouldBe expected.index
  }

  it should "work with empty series" in {
    val series = TimeSeries.empty[Int]
    series.sum shouldBe 0
    series.differentiate.values shouldBe series.values
  }

  it should "group by minute" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector(now, now.plusMinutes(1)), Vector(6, 9))

    series.groupByTime(_.withSecond(0), _.sum) shouldBe expected
  }

  it should "find sum in rolling windows operation" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(10), now.plusMinutes(30), now.plusMinutes(50), now.plusMinutes(80))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(idx, Vector(1,3,6,10,12))
    val window = Duration.ofHours(1)

    series.rollingWindow(window, _.sum) shouldBe expected
  }

}