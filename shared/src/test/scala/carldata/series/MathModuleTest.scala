package carldata.series

import carldata.series.MathModule._
import org.scalatest.{FlatSpec, Matchers}

class MathModuleTest extends FlatSpec with Matchers {
  "Math module" should "add constant to time series" in {
    val idx = Seq(1L, 2)
    val vals = Seq(4.0, 9.0)
    val ts = TimeSeries.fromTimestamps(idx.zip(vals))

    val const = 3.0
    val actual = ts.add(const)
    val expected = TimeSeries.fromTimestamps(idx.zip(vals.map(_ + const)))

    actual shouldBe expected
  }
}
