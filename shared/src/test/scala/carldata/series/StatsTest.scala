package carldata.series

import carldata.series.Stats.MeanAndVariance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StatsTest extends AnyFlatSpec with Matchers {

  "MeanAndVariance" should "calculate for non empty data" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val result = Stats.meanAndVariance(series.values)
    result.mean shouldEqual 4
    result.variance - 14.33333 < 0.0001 shouldBe true
  }

  it should "calculate for non empty data with sample population" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    val result = Stats.meanAndVariance(series.values,sample = true)
    result.mean shouldEqual 4
    result.variance - 17.2 < 0.0001 shouldBe true
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

  "Covariance" should "calculate easy case" in {
    val series1: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 2)))
    val series2: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 4)))
    Stats.covariance(series1.values, series2.values) shouldBe 0.75
  }

  "Correlation" should "calculate easy case" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4, 5)
    val ys: Seq[Double] = Seq(3, 4, 7, 2, 3)
    Stats.correlation(xs, ys) + 0.1644 < 0.0001 shouldBe true

  }
  "Stats" should "normalize series" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, -5), (2, 6), (3, 9), (4, 2), (5, 4)))
    val expected: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, -1.74), (2, 0.59), (3, 1.23), (4, -0.25), (5, 0.17)))
    val result = series.index.zip(Stats.normalize(series.values)).unzip
    val resultTs = TimeSeries(result._1, result._2)
    TimeSeries.almostEqual(resultTs, expected, 0.01) shouldBe true
  }

  it should "allow to find 25 percentile" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4)
    Stats.percentile(xs, 25) shouldBe 1.25
  }

  it should "allow to find median" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Stats.median(xs) shouldBe 3
  }

  it should "allow to find median #2" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4)
    Stats.median(xs) shouldBe 2.5
  }

  it should "allow to find 75 percentile" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4)
    Stats.percentile(xs, 75) shouldBe 3.75
  }

  it should "allow to find 100 percentile" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Stats.percentile(xs, 100) shouldBe 5
  }


}