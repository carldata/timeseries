package carldata.extras

import java.time._
import java.time.temporal.ChronoUnit

import carldata.series.TimeSeries

object FilterByDateModule {

  implicit class FilterByDateTimeSeriesOperations[V](ts: TimeSeries[V]) {
    private def getLastOf(unit: ChronoUnit): TimeSeries[V] = {
      ts.groupByTime(_.truncatedTo(unit), _.sortBy(_._1).unzip._2.last)
    }

    def getLastOfTheMinute: TimeSeries[V] = getLastOf(ChronoUnit.MINUTES)

    def getLastOfTheHour: TimeSeries[V] = getLastOf(ChronoUnit.HOURS)

    def getLastOfTheDay: TimeSeries[V] = getLastOf(ChronoUnit.DAYS)

    def getLastOfTheMonth(zoneOffset: ZoneOffset = ZoneOffset.UTC): TimeSeries[V] = {
      def truncateMonth(i: Instant): Instant = {
        LocalDateTime.ofInstant(i, zoneOffset)
          .withDayOfMonth(1)
          .withHour(0)
          .withMinute(0)
          .withSecond(0)
          .withNano(0).toInstant(zoneOffset)
      }

      ts.groupByTime(truncateMonth, _.sortBy(_._1).unzip._2.last)
    }

    def getLastOfTheYear(zoneOffset: ZoneOffset = ZoneOffset.UTC): TimeSeries[V] = {
      def truncateMonth(i: Instant): Instant = {
        LocalDateTime.ofInstant(i, zoneOffset)
          .withMonth(1)
          .withDayOfMonth(1)
          .withHour(0)
          .withMinute(0)
          .withSecond(0)
          .withNano(0).toInstant(zoneOffset)
      }

      ts.groupByTime(truncateMonth, _.sortBy(_._1).unzip._2.last)
    }
  }

}
