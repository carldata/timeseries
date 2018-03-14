package series

import java.time.Instant
import java.time.temporal.ChronoUnit

object Patterns {
  def daily[V](xs: TimeSeries[V])(implicit num: Fractional[V]): TimeSeries[V] = {
    /** Calculate seconds passed from midnight **/
    def fromMidnight(dt: Instant): Long = {
      dt.getEpochSecond - dt.truncatedTo(ChronoUnit.DAYS).getEpochSecond
    }

    def groupByDuration(dt: Instant): Instant = {
      Instant.EPOCH
        .truncatedTo(ChronoUnit.DAYS)
        .plusSeconds(fromMidnight(dt))
    }

    def average(vs: Seq[(Instant, V)]): V = num.div(vs.unzip._2.sum, num.fromInt(vs.length))

    xs.groupByTime(groupByDuration, average)
  }

}
