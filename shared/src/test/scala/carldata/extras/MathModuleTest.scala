package carldata.extras

import carldata.extras.MathModule._
import carldata.series.TimeSeries
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathModuleTest extends AnyFlatSpec with Matchers {

  "MathModule" should "add constant to each value" in {
    val idx = Seq(1L, 2)
    val vs1 = Seq(4.0, 9.0)
    val vs2 = Seq(7.0, 12.0)

    val ts = TimeSeries.fromTimestamps(idx.zip(vs1))
    val expected = TimeSeries.fromTimestamps(idx.zip(vs2))

    ts.add(3) shouldBe expected
  }

  it should "subtract each value" in {
    val idx = Seq(1L, 2)
    val vs1 = Seq(4.0, 9.0)
    val vs2 = Seq(1.0, 6.0)

    val ts = TimeSeries.fromTimestamps(idx.zip(vs1))
    val expected = TimeSeries.fromTimestamps(idx.zip(vs2))

    ts.subtract(3) shouldBe expected
  }

  it should "multiply each value by constant" in {
    val idx = Seq(1L, 2)
    val vs1 = Seq(4.0, 9.0)
    val vs2 = Seq(12.0, 27.0)

    val ts = TimeSeries.fromTimestamps(idx.zip(vs1))
    val expected = TimeSeries.fromTimestamps(idx.zip(vs2))

    ts.multiply(3) shouldBe expected
  }

  it should "divide each value by constant" in {
    val idx = Seq(1L, 2)
    val vs1 = Seq(4.0, 9.0)
    val vs2 = Seq(2.0, 4.5)

    val ts = TimeSeries.fromTimestamps(idx.zip(vs1))
    val expected = TimeSeries.fromTimestamps(idx.zip(vs2))

    ts.divide(2) shouldBe expected
  }

}
