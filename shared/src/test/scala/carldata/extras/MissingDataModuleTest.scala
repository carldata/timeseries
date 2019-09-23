package carldata.extras

import java.time.{Duration, Instant}

import carldata.series.TimeSeries
import org.scalatest.{FlatSpec, Matchers}
import carldata.extras.MissingDataModule._

class MissingDataModuleTest extends FlatSpec with Matchers {

  "MissingDataModule" should "fill missing points with forward fill" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val idx2 = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    val expected = TimeSeries(idx2, Vector(1f, 1f, 1f, 4f, 5f))
    series.forwardFill() shouldBe expected
  }

  it should "fill missing points with with backward fill" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(30), now.plusSeconds(40))
    val idx2 = Vector(now, now.plusSeconds(10), now.plusSeconds(20), now.plusSeconds(30), now.plusSeconds(40))
    val series = TimeSeries(idx, Vector(1f, 4f, 5f))
    val expected = TimeSeries(idx2, Vector(1f, 4f, 4f, 4f, 5f))
    series.backwardFill() shouldBe expected
  }

}
