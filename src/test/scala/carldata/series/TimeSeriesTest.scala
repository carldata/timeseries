package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import org.scalatest._


class TimeSeriesTest extends FlatSpec with Matchers {

  "TimeSeries" should "have length equal to its index" in {
    val now = LocalDateTime.now()
    val series = new TimeSeries(Seq((now, 1), (now.plusMinutes(1), 2), (now.plusMinutes(2), 3)))
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
    series.values.min shouldBe -3.4
  }

  it should "return maximum value" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.values.max shouldBe 5
  }

  it should "sum its elements" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.values.sum shouldBe 3
  }

  it should "map over its values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.map(x => x._2 + 2).values.max shouldBe 7.6
  }

  it should "mapValues over its values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.mapValues(_ + 2).values.max shouldBe 7.6
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
    TimeSeries.differentiate(series).values shouldBe expected.values
  }

  it should "differentiate with the overflow" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(5), now.plusMinutes(10), now.plusMinutes(15),
      now.plusMinutes(20), now.plusMinutes(25), now.plusMinutes(30))
    val series = TimeSeries(idx, Vector(1, 2, 3, 5, 8, 3, 2))
    val expected = TimeSeries(idx.tail, Vector(1, 1, 2, 3, 5, 9))
    TimeSeries.diffOverflow(series, 10).values shouldBe expected.values
  }

  it should "integrate values" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -2), (3, -10), (4, 2)))
    TimeSeries.integrate(series).values shouldBe expected.values
  }

  it should "integrate index" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -2), (3, -10), (4, 2)))
    TimeSeries.integrate(series).index shouldBe expected.index
  }

  it should "integrate by time" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(15), now.plusMinutes(30), now.plusMinutes(45),
      now.plusMinutes(60), now.plusMinutes(70))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5, 6))
    val expected = TimeSeries(idx, Vector(1, 3, 6, 10, 5, 11))

    TimeSeries.integrateByTime(series, Duration.ofHours(1)) shouldBe expected
  }

  it should "work with empty series" in {
    val series = TimeSeries.empty[Int]
    series.values.sum shouldBe 0
    TimeSeries.differentiate(series).values shouldBe series.values
  }

  it should "group by minute" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector(now, now.plusMinutes(1)), Vector(6, 9))

    series.groupByTime(_.withSecond(0), _.unzip._2.sum) shouldBe expected
  }

  it should "find sum in rolling windows operation" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(10), now.plusMinutes(30), now.plusMinutes(50), now.plusMinutes(80))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(idx, Vector(1, 3, 6, 10, 12))
    val window = Duration.ofHours(1)

    series.rollingWindow(window, _.sum) shouldBe expected
  }

  it should "interpolate: don't change series without missing values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(10), now.plusMinutes(20), now.plusMinutes(30), now.plusMinutes(40))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 4f, 5f))
    val expected = TimeSeries(idx, Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofMinutes(10)) shouldBe expected
  }

  it should "interpolate: missing values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(5))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 5f))
    val expected = TimeSeries(Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(4), now.plusMinutes(5)), Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "fill missing: missing values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(5))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 5f))
    val expected = TimeSeries(Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(4), now.plusMinutes(5)), Vector(1f, 2f, 3f, 1f, 5f))

    TimeSeries.fillMissing(series, Duration.ofMinutes(1), 1f) shouldBe expected
  }

  it should "interpolate: missing lots of values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now.plusMinutes(1), now.plusMinutes(5))
    val series = TimeSeries(idx, Vector(1f, 5f))
    val expected = TimeSeries(Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(4), now.plusMinutes(5)), Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "interpolate: middle values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now.plusMinutes(1), now.plusMinutes(4), now.plusMinutes(6))
    val series = TimeSeries(idx, Vector(1f, 4f, 6f))
    val expected = TimeSeries(Vector(now.plusMinutes(1), now.plusMinutes(3), now.plusMinutes(5)), Vector(1f, 3f, 5f))

    TimeSeries.interpolate(series, Duration.ofMinutes(2)) shouldBe expected
  }

  it should "interpolate: empty series" in {
    val emptySeries = TimeSeries.empty[Float]
    TimeSeries.interpolate(emptySeries, Duration.ofMinutes(2)) shouldBe emptySeries
  }

  it should "repeat values" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(15), now.plusMinutes(30), now.plusMinutes(45))
    val series = TimeSeries(idx, Vector(1, 4, 6, 8))
    val idx2 = Vector(now.plusMinutes(60), now.plusMinutes(75), now.plusMinutes(90), now.plusMinutes(105))
    val expected = TimeSeries(idx ++ idx2, Vector(1, 4, 6, 8, 1, 4, 6, 8))

    series.repeat(now, now.plusHours(2), Duration.ofHours(1)) shouldBe expected
  }

  it should "shift time forward" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusMinutes(15), now.plusMinutes(30), now.plusMinutes(45))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(now.plusMinutes(60), now.plusMinutes(75), now.plusMinutes(90), now.plusMinutes(105))
    val expected = TimeSeries(idx2, vs)

    series.shiftTime(Duration.ofHours(1), forward = true) shouldBe expected
  }

  it should "shift time backward" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now.plusMinutes(60), now.plusMinutes(75), now.plusMinutes(90), now.plusMinutes(105))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(now, now.plusMinutes(15), now.plusMinutes(30), now.plusMinutes(45))
    val expected = TimeSeries(idx2, vs)

    series.shiftTime(Duration.ofHours(1), forward = false) shouldBe expected
  }

  it should "step index" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx = Vector(now, now.plusHours(1), now.plusHours(2))
    val vs = Vector(10f, 8f, 12f)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(
      now, now.plusMinutes(15), now.plusMinutes(30), now.plusMinutes(45),
      now.plusMinutes(60), now.plusMinutes(75), now.plusMinutes(90), now.plusMinutes(105))
    val expected = TimeSeries(idx2, Vector(2, 2, 2, 2, 3, 3, 3, 3))

    TimeSeries.step(series, Duration.ofMinutes(15)) shouldBe expected
  }

  it should "inner join 2 series" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx1 = Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(4), now.plusMinutes(5))
    val idx2 = Vector(now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(4), now.plusMinutes(5))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val idx3 = Vector(now.plusMinutes(2), now.plusMinutes(4), now.plusMinutes(5))
    val expected = TimeSeries(idx3, Vector((4, 1), (6, 6), (8, 8)))

    series1.join(series2) shouldBe expected
  }

  it should "left join 2 series" in {
    val now = LocalDateTime.parse("2015-01-01T00:00:00")
    val idx1 = Vector(now.plusMinutes(1), now.plusMinutes(2), now.plusMinutes(4), now.plusMinutes(5))
    val idx2 = Vector(now.plusMinutes(2), now.plusMinutes(3), now.plusMinutes(4), now.plusMinutes(5))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val expected = TimeSeries(idx1, Vector((1, 0), (4, 1), (6, 6), (8, 8)))

    series1.joinLeft(series2, 0) shouldBe expected
  }

}