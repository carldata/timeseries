package carldata.extras

import java.time.{Duration, Instant}

import carldata.extras.MissingDataModule._
import carldata.series.TimeSeries
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MissingDataModuleTest extends AnyFlatSpec with Matchers {

  "MissingDataModule" should "fill missing points with forward fill" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val idx2 = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    val expected = TimeSeries(idx2, Vector(1f, 1f, 1f, 4f, 5f))
    series.forwardFill(Duration.ofSeconds(10)) shouldBe expected
  }

  it should "fill missing points with with backward fill" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val idx2 = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    val expected = TimeSeries(idx2, Vector(1f, 4f, 4f, 4f, 5f))
    series.backwardFill(Duration.ofSeconds(10)) shouldBe expected
  }

  it should "fill missing points with locf" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val idx2 = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    val expected = TimeSeries(idx2, Vector(1f, 1f, 1f, 4f, 5f))
    series.locf(Duration.ofSeconds(10)) shouldBe expected
  }

  it should "fill missing points with locf with small max gap" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    series.locf(series.resolution, Duration.ofSeconds(20)) shouldBe series
  }

  it should "fill missing points with locf with small max gap and two gaps" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40)
      , now.plusSeconds(60), now.plusSeconds(70))
    val idx2 = Vector(now, now.plusSeconds(30), now.plusSeconds(40)
      , now.plusSeconds(50), now.plusSeconds(60), now.plusSeconds(70))
    val series = TimeSeries(idx, Vector(1f, 4f, 4f, 5f, 6f))
    val expected = TimeSeries(idx2, Vector(1f, 4f, 4f, 4f, 5f, 6f))
    series.locf(series.resolution, Duration.ofSeconds(20)) shouldBe expected
  }

}
