package series

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class PatternsTest extends FlatSpec with Matchers {
  "Patterns" should "estimate daily pattern" in {
    val now = Instant.EPOCH
    val day = 86400
    val idx = Vector(now, now.plusSeconds(300), now.plusSeconds(600)
      , now.plusSeconds(day), now.plusSeconds(day + 300), now.plusSeconds(day + 600)
      , now.plusSeconds(2 * day), now.plusSeconds(2 * day + 300), now.plusSeconds(2 * day + 600))
    val series = TimeSeries(idx, Vector(1d, 2d, 3d, 2d, 4d, 6d, 3d, 6d, 9d))
    val expected = TimeSeries(Vector(now, now.plusSeconds(300), now.plusSeconds(600)), Vector(2d, 4d, 6d))
    val result = Patterns.daily(series)
    result shouldBe expected
  }
}
