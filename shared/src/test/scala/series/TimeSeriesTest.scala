package series

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}

import org.scalatest._


class TimeSeriesTest extends FlatSpec with Matchers {

  "TimeSeries" should "have length equal to its index" in {
    val now = Instant.now()
    val series = new TimeSeries(Seq((now, 1), (now.plusSeconds(1), 2), (now.plusSeconds(2), 3)))
    series.length shouldBe 3
  }

  it should "be build from the rows" in {
    val now = Instant.now()
    val series = TimeSeries(Vector(now, now.plusSeconds(1), now.plusSeconds(2)), Vector(1, 2, 3, 4, 5.6))
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
    val now = Instant.now()
    val series = TimeSeries(Vector(now, now.plusSeconds(1), now.plusSeconds(2)), Vector(1, 2, 3, 4, 5.6))
    series.head shouldBe Some((now, 1))
  }

  it should "return None as first element for empty Series" in {
    val series = TimeSeries.empty[Int]
    series.head shouldBe None
  }

  it should "return last element of a Series" in {
    val now = Instant.now()
    val series = TimeSeries(Vector(now, now.plusSeconds(1), now.plusSeconds(2)), Vector(1, 2, 3, 4, 5.6))
    series.last shouldBe Some((now.plusSeconds(2), 3))
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

  it should "sort series by index" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 2), (5, 5), (3, 3), (4, 4), (6, 6)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 2), (5, 5), (6, 6)))
    series.sortByIndex shouldBe expected
  }

  it should "return sub series" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 6)))
    val start = Instant.ofEpochSecond(2)
    val end = Instant.ofEpochSecond(6)
    series.slice(start, end).length shouldBe 4
  }

  it should "take first n element" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 6)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6)))
    series.take(4) shouldBe expected
  }

  it should "differentiate" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((2, -6), (3, -2), (4, 14)))
    TimeSeries.differentiate(series).values shouldBe expected.values
  }

  it should "differentiate with the overflow" in {
    val now = Instant.now()
    val idx = Vector(now, now.plusSeconds(5), now.plusSeconds(10), now.plusSeconds(15),
      now.plusSeconds(20), now.plusSeconds(25), now.plusSeconds(30))
    val series = TimeSeries(idx, Vector(1, 2, 3, 5, 8, 3, 2))
    val expected = TimeSeries(idx.tail, Vector(1, 1, 2, 3, 5, 9))
    TimeSeries.diffOverflow(series, 10) shouldBe expected
  }

  it should "differentiate with the overflow without overflowValue" in {
    val now = Instant.now()
    val idx = Vector(now, now.plusSeconds(5), now.plusSeconds(10), now.plusSeconds(15),
      now.plusSeconds(20), now.plusSeconds(25), now.plusSeconds(30))
    val series = TimeSeries(idx, Vector(1, 2, 3, 3, 8, 3, 2))
    val expected = TimeSeries(idx, Vector(1, 1, 1, 0, 5, 3, 2))
    TimeSeries.diffOverflow(series) shouldBe expected
  }

  it should "differentiate empty series" in {
    val series = TimeSeries.empty[Int]
    series.values.sum shouldBe 0
    TimeSeries.differentiate(series).values shouldBe series.values
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
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(idx, Vector(1, 3, 6, 4, 9))

    TimeSeries.integrateByTime(series, _.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }

  it should "group by minute" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(6, 9))

    series.groupByTime(_.truncatedTo(ChronoUnit.MINUTES), _.unzip._2.sum) shouldBe expected
  }

  it should "group by hour" in {
    def groupHours(dt: Instant): Instant = {
      Instant.EPOCH.truncatedTo(ChronoUnit.HOURS).plusSeconds(LocalDateTime.ofInstant(dt, ZoneOffset.UTC).getSecond)
    }

    val now = Instant.EPOCH
    val day = 86400
    val idx = Vector(now, now.plusSeconds(10), now.plusSeconds(20)
      , now.plusSeconds(day), now.plusSeconds(day + 10), now.plusSeconds(day + 20)
      , now.plusSeconds(2 * day), now.plusSeconds(2 * day + 10), now.plusSeconds(2 * day + 20))
    val series = TimeSeries(idx, Vector(1, 2, 3, 1, 2, 3, 1, 2, 4))
    val expected = TimeSeries(Vector(now, now.plusSeconds(10), now.plusSeconds(20)), Vector(3, 6, 10))
    series.groupByTime(groupHours, _.unzip._2.sum) shouldBe expected
  }

  it should "find sum in rolling windows operation" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(10 * 60), now.plusSeconds(30 * 60), now.plusSeconds(50 * 60),
      now.plusSeconds(80 * 60))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(idx, Vector(1, 3, 6, 10, 12))
    val window = Duration.ofHours(1)
    println(window)
    series.rollingWindow(window, _.sum) shouldBe expected
  }

  it should "interpolate: don't change series without missing values" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1, 2f, 3f, 4f, 5f))
    val expected = TimeSeries(idx, Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofSeconds(10)) shouldBe expected
  }

  it should "interpolate: missing values" in {
    val now = Instant.EPOCH
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(5))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 5f))
    val expected = TimeSeries(Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4),
      now.plusSeconds(5)), Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofSeconds(1)) shouldBe expected
  }

  it should "interpolate: missing lots of values" in {
    val now = Instant.EPOCH
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(5))
    val series = TimeSeries(idx, Vector(1f, 5f))
    val expected = TimeSeries(Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3),
      now.plusSeconds(4), now.plusSeconds(5)), Vector(1f, 2f, 3f, 4f, 5f))

    TimeSeries.interpolate(series, Duration.ofSeconds(1)) shouldBe expected
  }

  it should "interpolate: middle values" in {
    val now = Instant.EPOCH
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(4), now.plusSeconds(6))
    val series = TimeSeries(idx, Vector(1f, 4f, 6f))
    val expected = TimeSeries(Vector(now.plusSeconds(1), now.plusSeconds(3), now.plusSeconds(5)),
      Vector(1f, 3f, 5f))

    TimeSeries.interpolate(series, Duration.ofSeconds(2)) shouldBe expected
  }

  it should "interpolate: empty series" in {
    val emptySeries = TimeSeries.empty[Float]
    TimeSeries.interpolate(emptySeries, Duration.ofMinutes(2)) shouldBe emptySeries
  }

  it should "resample with default value" in {
    val now = Instant.EPOCH
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(5))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 5f))
    val expected = TimeSeries(Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3),
      now.plusSeconds(4), now.plusSeconds(5)), Vector(1f, 2f, 3f, 1f, 5f))

    series.resampleWithDefault(Duration.ofSeconds(1), 1f) shouldBe expected
  }

  it should "add missing values" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(15), now.plusSeconds(30), now.plusSeconds(45),
      now.plusSeconds(65), now.plusSeconds(80))
    val series = TimeSeries(idx, Vector(1f, 2f, 3f, 3f, 2f, 6f))
    val idx2 = Vector(now, now.plusSeconds(15), now.plusSeconds(30), now.plusSeconds(45),
      now.plusSeconds(60), now.plusSeconds(65), now.plusSeconds(80))
    val expected = TimeSeries(idx2, Vector(1f, 2f, 3f, 3f, 3f, 2f, 6f))

    def f(x1: (Instant, Float), x2: (Instant, Float), tsh: Instant) = x1._2

    series.addMissing(Duration.ofMinutes(1), f) shouldBe expected
  }

  it should "shift time forward" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(15), now.plusSeconds(30), now.plusSeconds(45))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(now.plusSeconds(60), now.plusSeconds(75), now.plusSeconds(90), now.plusSeconds(105))
    val expected = TimeSeries(idx2, vs)

    series.shiftTime(Duration.ofMinutes(1)) shouldBe expected
  }

  it should "shift time backward" in {
    val now = Instant.EPOCH
    val idx = Vector(now.plusSeconds(60), now.plusSeconds(75), now.plusSeconds(90), now.plusSeconds(105))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(now, now.plusSeconds(15), now.plusSeconds(30), now.plusSeconds(45))
    val expected = TimeSeries(idx2, vs)
    series.shiftTime(Duration.ofMinutes(-1)) shouldBe expected
  }

  it should "step index" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(60), now.plusSeconds(2 * 60))
    val vs = Vector(10f, 8f, 12f)
    val series = TimeSeries(idx, vs)
    val idx2 = Vector(now, now.plusSeconds(15), now.plusSeconds(30), now.plusSeconds(45),
      now.plusSeconds(60), now.plusSeconds(75), now.plusSeconds(90), now.plusSeconds(105))
    val expected = TimeSeries(idx2, Vector(2, 2, 2, 2, 3, 3, 3, 3))

    TimeSeries.step(series, Duration.ofSeconds(15)) shouldBe expected
  }

  it should "inner join 2 series" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val idx3 = Vector(now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val expected = TimeSeries(idx3, Vector((4, 1), (6, 6), (8, 8)))

    series1.join(series2) shouldBe expected
  }

  it should "inner join 4 series" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val idx3 = Vector(now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5), now.plusSeconds(6))
    val idx4 = Vector(now.plusSeconds(4), now.plusSeconds(5), now.plusSeconds(6), now.plusSeconds(7))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val series3 = TimeSeries(idx3, vs)
    val series4 = TimeSeries(idx4, vs)
    val idx5 = Vector(now.plusSeconds(4), now.plusSeconds(5))
    val expected = TimeSeries(idx5, Vector(List(6, 6, 4, 1), List(8, 8, 6, 4)))

    TimeSeries.join(Seq(series1, series2, series3, series4)) shouldBe expected
  }

  it should "left join 2 series" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val expected = TimeSeries(idx1, Vector((1, 0), (4, 1), (6, 6), (8, 8)))

    series1.joinLeft(series2, 0) shouldBe expected
  }

  it should "outer join 2 series" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val idx3 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4),
      now.plusSeconds(5))
    val vs = Vector(1, 4, 6, 8)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs)
    val expected = TimeSeries(idx3, Vector((1, -2), (4, 1), (-1, 4), (6, 6), (8, 8)))

    series1.joinOuter(series2, -1, -2) shouldBe expected
  }

  it should "outer join 2 series(one empty)" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1f, 4f, 6f, 8f)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries.empty[Float]
    val expected = TimeSeries(idx1, Vector((1f, -2f), (4f, -2f), (6f, -2f), (8f, -2f)))

    series1.joinOuter(series2, -1f, -2f) shouldBe expected
  }

  it should "merge 2 series" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val vs1 = Vector(1, 4, 6, 8)
    val vs2 = Vector(2, 3, 5, 9)
    val series1 = TimeSeries(idx1, vs1)
    val series2 = TimeSeries(idx2, vs2)
    val idx3 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val expected = TimeSeries(idx3, Vector(1, 4, 3, 6, 8))

    series1.merge(series2) shouldBe expected
  }

  it should "check if 2 ts is almost equal (true)" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1.2005, 1.000054, 1.000034, 1.000022)
    val vs2 = Vector(1.2005, 1.000053, 1.000030, 1.000020)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs2)

    TimeSeries.almostEqual(series1, series2, 0.00001) shouldBe true
  }

  it should "check if 2 ts is almost equal (false)" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1.005f, 1.000054f, 1.000034f, 1.000022f)
    val vs2 = Vector(1.2005f, 1.000053f, 1.000030f, 1.000020f)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs2)

    TimeSeries.almostEqual(series1, series2, 0.00001f) shouldBe false
  }
  it should "check if 2 ts is almost equal - extra size" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now, now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1f, 2.2f, 2.4f, 10.7f)
    val vs2 = Vector(0.5f, 1f, 2.2f, 2.4f, 10.7f)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs2)

    TimeSeries.almostEqual(series1, series2, 0.01f) shouldBe false
  }

  it should "check if 2 ts is almost equal- different index" in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(5))
    val idx2 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(4), now.plusSeconds(6))
    val vs = Vector(1.2005f, 1.000054f, 1.000034f, 1.000022f)
    val vs2 = Vector(1.2005f, 1.000053f, 1.000030f, 1.000020f)
    val series1 = TimeSeries(idx1, vs)
    val series2 = TimeSeries(idx2, vs2)

    TimeSeries.almostEqual(series1, series2, 0.00001f) shouldBe false
  }

  it should "find most frequent duration" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(60), now.plusSeconds(80))
    val series = TimeSeries(idx, Vector(1, 2f, 3f, 4f, 5f))
    val expected = Duration.of(10l, ChronoUnit.SECONDS)
    series.resolution shouldBe expected
  }


}