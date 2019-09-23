package carldata.extras

import java.time.{Duration, Instant}

import carldata.series.TimeSeries

object MissingDataModule {

  implicit class MissingDataTimeSeriesOperations[V](ts: TimeSeries[V]) {
    /** Filling missing data with last known value
      * @param delta Expected distance between points
      */
    def forwardFill(delta: Duration = ts.resolution): TimeSeries[V] = {
      def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant): V = x1._2

      ts.addMissing(ts.resolution, f)
    }
    /** Filling missing data with next known value
      * @param delta Expected distance between points
      */
    def backwardFill(delta: Duration = ts.resolution): TimeSeries[V] = {
      def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant): V = x2._2

      ts.addMissing(ts.resolution, f)
    }
  }


}
