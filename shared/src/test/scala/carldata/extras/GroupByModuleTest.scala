package carldata.extras

import java.time.Instant
import java.time.temporal.ChronoUnit

import carldata.extras.GroupByModule._
import carldata.series.TimeSeries
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GroupByModuleTest extends AnyFlatSpec with Matchers {

  "GroupByModule" should "group by sum" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(6, 9))

    series.groupBySum(_.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }

  it should "group by average" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 4d, 5d))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(2, 4.5))

    series.groupByAvg(_.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }
  it should "group by max" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 4d, 5d))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(3, 5))

    series.groupByMax(_.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }
  it should "group by min" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 4d, 5d))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(1, 4))

    series.groupByMin(_.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }
  it should "group by median" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(34), now.plusSeconds(44), now.plusSeconds(65), now.plusSeconds(74))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 4d, 5d))
    val expected = TimeSeries(Vector(now, now.plusSeconds(60)), Vector(2, 4.5))

    series.groupByMedian(_.truncatedTo(ChronoUnit.MINUTES)) shouldBe expected
  }
}
