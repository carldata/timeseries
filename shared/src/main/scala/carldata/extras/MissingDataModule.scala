package carldata.extras

import java.time.{Duration, Instant}

import carldata.series.{Gen, TimeSeries}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MissingDataModule {

  implicit class MissingDataTimeSeriesOperations[V](ts: TimeSeries[V]) {
    /** Filling missing data with last known value
      *
      * @param delta Expected distance between points
      */
    def forwardFill(delta: Duration = ts.resolution): TimeSeries[V] = {
      def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant): V = x1._2

      ts.addMissing(ts.resolution, f)
    }

    /** Filling missing data with next known value
      *
      * @param delta Expected distance between points
      */
    def backwardFill(delta: Duration = ts.resolution): TimeSeries[V] = {
      def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant): V = x2._2

      ts.addMissing(ts.resolution, f)
    }

    /** Filling missing data with last known value.
      * Last observation carried forward.
      *
      * @param maxGap size of space (of missing points) after which we decide to drop filling missing points
      *               (i.e. inserting measurement from last month may have no sense).
      */
    def locf(maxGap: Duration = Duration.ofDays(365))(implicit num: Fractional[V]): TimeSeries[V] = {
      if (ts.isEmpty) ts
      else {
        val resampledIndex = Gen.mkIndex(ts.index.head, ts.index.last, ts.resolution)

        @tailrec def g(idxs: Vector[Instant], xs: Vector[(Instant, V)]
                       , prev: (Instant, V), res: ListBuffer[(Instant, V)]): ListBuffer[(Instant, V)] = {
          if (xs.nonEmpty) {
            val xsh = xs.head
            if (idxs.isEmpty) {
              g(idxs, xs.tail, xsh, res :+ xs.head)
            } else if (xsh._1 == idxs.head) {
              g(idxs.tail, xs.tail, xsh, res :+ xs.head)
            }
            else if (idxs.head.isBefore(xsh._1)) {
              if (prev._1.plus(maxGap).isAfter(xsh._1.minusNanos(1))) {
                val dp = (idxs.head, prev._2)
                g(idxs.tail, xs, dp, res :+ dp)
              }
              else g(idxs.tail, xs, prev, res)
            }
            else g(idxs, xs.tail, xsh, res :+ xs.head)
          }
          else res
        }


        val dp = ts.dataPoints
        val res = g(resampledIndex, dp, dp.head, ListBuffer())
        new TimeSeries(res.toSeq)
      }
    }
  }


}
