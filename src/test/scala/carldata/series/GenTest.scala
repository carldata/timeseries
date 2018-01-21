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


}