package carldata.series

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, ZoneOffset}

object Patterns {
  def daily[V](xs: TimeSeries[V])(implicit num: Fractional[V]): TimeSeries[V] = {
    def groupBy5Minutes(dt: Instant): Instant = {
      val h = LocalDateTime.ofInstant(dt, ZoneOffset.UTC).getHour
      val m = Math.floorDiv(LocalDateTime.ofInstant(dt, ZoneOffset.UTC).getMinute, 5) * 5
      Instant.EPOCH.truncatedTo(ChronoUnit.HOURS).plusSeconds(h * 3600).plusSeconds(m * 60)
    }

    def average(vs: Seq[(Instant, V)]): V = num.div(vs.unzip._2.sum, num.fromInt(vs.length))

    xs.groupByTime(groupBy5Minutes, average)
  }

}
