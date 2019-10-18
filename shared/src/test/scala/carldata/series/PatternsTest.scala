package carldata.series

import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PatternsTest extends AnyFlatSpec with Matchers {
  "Patterns" should "estimate daily pattern" in {
    val now = Instant.EPOCH
    val day = 86400
    val idx = Vector(now, now.plusSeconds(300), now.plusSeconds(600)
      , now.plusSeconds(day), now.plusSeconds(day + 300), now.plusSeconds(day + 600)
      , now.plusSeconds(2 * day), now.plusSeconds(2 * day + 300), now.plusSeconds(2 * day + 600))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 2d, 4d, 6d, 3d, 6d, 9d))
    val expectedTS = TimeSeries(Vector(now, now.plusSeconds(300), now.plusSeconds(600))
      , Vector(2d, 4d, 6d))
    val expectedSD = TimeSeries(Vector(now, now.plusSeconds(300), now.plusSeconds(600))
      , Vector(0.81649d, 1.63299d, 2.44948d))
    val result = Patterns.daily(series)
    result.mapValues(_._1) shouldBe expectedTS
    TimeSeries.almostEqual(result.mapValues(_._2), expectedSD, 0.01) shouldBe true
  }
}
