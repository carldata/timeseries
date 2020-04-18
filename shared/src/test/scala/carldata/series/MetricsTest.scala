package carldata.series

import java.time.Instant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MetricsTest extends AnyFlatSpec with Matchers {
  "Metrics" should "calculate mae on series with odd length" in {
    val idx: Vector[Instant] = (0 until 5).map(Instant.EPOCH.plusSeconds(_)).toVector
    val series1 = TimeSeries(idx, Vector(1d, 2d, 3d, 2d, 4d))
    val series2 = TimeSeries(idx, Vector(2d, 4d, 5d, 5d, 5d))
    Metrics.mae(series1, series2) shouldEqual 2
  }

  it should "calculate mae on series with even length" in {
    val idx: Vector[Instant] = (0 until 5).map(Instant.EPOCH.plusSeconds(_)).toVector
    val series1 = TimeSeries(idx, Vector(1d, 2d, 3d, 2d, 4d))
    val series2 = TimeSeries(idx.tail, Vector(2d, 4d, 5d, 5d))
    Metrics.mae(series1, series2) shouldEqual 1
  }

  it should "calculate rmse on series with the same index" in {
    val idx: Vector[Instant] = (0 to 6).map(Instant.EPOCH.plusSeconds(_)).toVector
    val vs1 = Vector(0d, 1d, 2d, 3d, 4d, 5d, 6d)
    val vs2 = Vector(0d, 1d, 1d, 2d, 3d, 5d, 8d)
    Metrics.rmse(TimeSeries(idx, vs1), TimeSeries(idx, vs2)) shouldEqual 1d
  }
  it should "give correct values for mad on series with the same index" in {
    val idx: Vector[Instant] = (for (i <- 0 to 6) yield i).map(Instant.MIN.plusSeconds(_)).toVector
    val vs1 = Vector(2d, 2d, 3d, 4d, 14d)
    val vs2 = Vector(5d, 5d, 5d, 5d, 5d)
    Metrics.mad(TimeSeries(idx, vs1), TimeSeries(idx, vs2)) shouldEqual 3.6d
  }

  it should "give correct values for mape on series with the same index" in {
    val idx: Vector[Instant] = (for (i <- 0 to 6) yield i).map(Instant.MIN.plusSeconds(_)).toVector
    val vs1 = Vector(1d, 2d, 3d, 4d, 5d)
    val vs2 = Vector(0d, 0d, 3d, 2d, 5d)
    Metrics.mape(TimeSeries(idx, vs1), TimeSeries(idx, vs2)) shouldEqual 50d
    Metrics.mape(TimeSeries(idx, vs2), TimeSeries(idx, vs1)) shouldEqual 20d
  }

}
