package carldata.series

import java.time.{Duration, Instant}

import org.scalatest._


class GenTest extends FlatSpec with Matchers {

  "Constant generator" should "create series with constant value." in {
    val idx = Gen.mkIndex(Instant.ofEpochSecond(1), Instant.ofEpochSecond(5), Duration.ofSeconds(1))
    val expected = TimeSeries(idx, Vector(1, 1, 1, 1, 1))
    val result = Gen.constant(idx, 1)
    result shouldBe expected
  }

  "Repeat generator" should "create series when start date is before pattern." in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4))
    val idx2 = Vector(now.minusSeconds(2), now.minusSeconds(1), now,
      now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4), now.plusSeconds(5))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx1, vs)
    val expected = TimeSeries(idx2, vs ++ vs)
    val result = Gen.repeat(series, now.minusSeconds(2), now.plusSeconds(5))
    result shouldBe expected
  }

  it should "create series when start date is after pattern." in {
    val now = Instant.now()
    val idx1 = Vector(now.plusSeconds(1), now.plusSeconds(2), now.plusSeconds(3), now.plusSeconds(4))
    val idx2 = Vector(now.plusSeconds(11), now.plusSeconds(12), now.plusSeconds(13), now.plusSeconds(14),
      now.plusSeconds(15), now.plusSeconds(16), now.plusSeconds(17), now.plusSeconds(18))
    val vs = Vector(1, 4, 6, 8)
    val series = TimeSeries(idx1, vs)
    val expected = TimeSeries(idx2, vs ++ vs)
    val result = Gen.repeat(series, now.plusSeconds(11), now.plusSeconds(18))
    result shouldBe expected
  }


}