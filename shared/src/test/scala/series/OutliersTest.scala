package series

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class OutliersTest extends FlatSpec with Matchers {

  "Outliers module" should "remove outliers" in {
    val now = Instant.now()
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5),
      now.plusSeconds(6))
    val idx2 = Vector(now.plusSeconds(1), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(6))
    val series = TimeSeries(idx, Vector(3, 20, 5, 6, 0, 8))
    val expected = TimeSeries(idx2, Vector(3, 5, 6, 8))

    Outliers.remove(series, 1, 10) shouldBe expected
  }


  it should "trim outliers" in {
    val now = Instant.EPOCH
    val idx = Vector(now, now.plusSeconds(10), now.plusSeconds(30), now.plusSeconds(50), now.plusSeconds(80))
    val series = TimeSeries(idx, Vector(1, 200, -3, 4, 5))
    val expected = TimeSeries(idx, Vector(1, 5, 0, 4, 5))

    Outliers.trim(series, 0, 5) shouldBe expected
  }

  it should "interpolate outliers" in {
    def f(x: Float, y: Float): Float = (x + y) / 2

    val now = Instant.now()
    val idx = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4),
      now.plusSeconds(5), now.plusSeconds(6))
    val series = TimeSeries[Float](idx, Vector(3f, 20f, 5f, 6f, 0f, 8f))
    val expected = TimeSeries(idx, Vector(3, 4, 5, 6, 7, 8))
    
    Outliers.interpolate(series, 1f, 10f, f) shouldBe expected
  }
}

