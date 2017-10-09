package carldata.series

import carldata.series.Stats.MeanAndVariance
import org.scalatest._



class StatsTest extends FlatSpec with Matchers {

  "MeanAndVariance" should "calculate for non empty data." in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val result = Stats.meanAndVariance(series.values)
    result.mean shouldEqual 4
    result.variance - 14.33333 < 0.0001 shouldBe true
  }

  it should "work with empty series" in {
    val result = Stats.meanAndVariance[Double](Vector.empty)
    result shouldBe MeanAndVariance(0, 0)
  }

  "AutoCovariance" should "equal to variance at lag == 0" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val mv = Stats.meanAndVariance(series.values)
    mv.variance shouldBe Stats.autoCovariance(series.values, 0, 0, 6, mv.mean)
  }

  it should "calculate easy case" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val mv = Stats.meanAndVariance(series.values)
    Stats.autoCovariance(series.values, 0, 2, 2, mv.mean) shouldBe -10
  }

  it should "not depend which is starting index" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val mv = Stats.meanAndVariance(series.values)
    Stats.autoCovariance(series.values, 3, 0, 3, mv.mean) shouldBe Stats.autoCovariance(series.values, 0, 3, 3, mv.mean)
  }

  "AutoCorrelation" should "equal to 1 at lag == 0" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    Stats.autoCorrelation(series.values, 0, 0, 6) shouldBe 1
  }

  "AutoCorrelation" should "calculate easy case" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    Stats.autoCorrelation(series.values, 0, 2, 2) + 0.3448275862 < 0.0001 shouldBe true
  }

  it should "work with empty series" in {
    Stats.autoCorrelation[Double](Seq(), 0, 2, 2) shouldBe 0
  }

}