package carldata.series

import java.time.Instant
import java.time.temporal.ChronoUnit

import carldata.series.Stats.meanAndVariance

object Patterns {
  def daily[V: Fractional](xs: TimeSeries[V])(implicit num: Fractional[V]): TimeSeries[(Double, Double)] = {
    val xs2 = TimeSeries(xs.index, xs.values.map(num.toDouble))

    /** Calculate seconds passed from midnight **/
    def fromMidnight(dt: Instant): Long = {
      dt.getEpochSecond - dt.truncatedTo(ChronoUnit.DAYS).getEpochSecond
    }

    def groupByDuration(dt: Instant): Instant = {
      Instant.EPOCH
        .truncatedTo(ChronoUnit.DAYS)
        .plusSeconds(fromMidnight(dt))
    }

    def average(vs: Seq[(Instant, Double)]): Double = Stats.meanAndVariance(vs.unzip._2).mean

    def standardDeviation(vs: Seq[(Instant, Double)]): Double = {
      val variance = meanAndVariance(vs.unzip._2).variance
      Math.sqrt(variance)
    }

    val mean = xs2.groupByTime(groupByDuration, average)
    val stddev = xs2.groupByTime(groupByDuration, standardDeviation)
    mean.join(stddev)
  }

}
