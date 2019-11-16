package carldata.extras

import java.time.Instant

import carldata.series.TimeSeries

object GroupByModule {

  implicit class GroupByTimeSeriesOperations[V](ts: TimeSeries[V]) {
    /**
      * Sum data points.
      *
      * @param g This function transforms current data point time into new time. All points with the same
      *          time will be integrated into single point
      */
    def groupBySum(g: Instant => Instant)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.groupByTime(g, _.unzip._2.sum)
    }

    /**
      * Find average of data points.
      *
      * @param g This function transforms current data point time into new time. All points with the same
      *          time will be integrated into single point
      */
    def groupByAvg(g: Instant => Instant)(implicit num: Fractional[V]): TimeSeries[V] = {
      def f(xs: Seq[(Instant, V)])(implicit num: Fractional[V]): V = {
        num.div(xs.unzip._2.sum, num.fromInt(xs.length))
      }

      ts.groupByTime(g, f)
    }

    /**
      * Find max of data points.
      *
      * @param g This function transforms current data point time into new time. All points with the same
      *          time will be integrated into single point
      */
    def groupByMax(g: Instant => Instant)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.groupByTime(g, _.unzip._2.max)
    }

    /**
      * Find min of data points.
      *
      * @param g This function transforms current data point time into new time. All points with the same
      *          time will be integrated into single point
      */
    def groupByMin(g: Instant => Instant)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.groupByTime(g, _.unzip._2.min)
    }

    /**
      * Find median of data points.
      *
      * @param g This function transforms current data point time into new time. All points with the same
      *          time will be integrated into single point
      */
    def groupByMedian(g: Instant => Instant)(implicit num: Fractional[V]): TimeSeries[V] = {
      def f(seq: Seq[V]): V = {
        val sorted: Seq[V] = seq.sorted

        val center = Math.abs(sorted.length / 2)
        if (seq.length % 2 == 0) {
          num.div(num.plus(sorted(center), sorted(center - 1)), num.fromInt(2))
        }
        else {
          sorted(center)
        }
      }

      if (ts.isEmpty) ts
      else {
        ts.groupByTime(g, x => f(x.unzip._2))
      }
    }

    /**
      * Find mode of data points.
      *
      * @param g              This function transforms current data point time into new time. All points with the same
      *                       time will be integrated into single point
      * @param modeSubstitute Function which tell how to deal with situation when mode not exist.
      *                       (for example when all values occur with the same frequency)
      */
    def groupByMode(g: Instant => Instant
                    , modeSubstitute: Seq[(Instant, V)] => V = (xs: Seq[(Instant, V)]) => mostFrequent(xs).head
                   ): TimeSeries[V] = {
      def mode(xs: Seq[(Instant, V)]): V = {
        val ys = mostFrequent(xs)
        if (ys.length == 1) ys.head
        else modeSubstitute(xs)
      }

      if (ts.isEmpty) ts
      else {
        ts.groupByTime(g, mode)
      }
    }

    /**
      * Helper function for groupByMode
      *
      * @param xs some window in series to be grouped
      * @return list of values which have the same number of occurences
      */
    def mostFrequent(xs: Seq[(Instant, V)]): Seq[V] = {
      val valueWithOccurences = xs.map(_._2).distinct
        .map(value => (value, xs.count(x => x._2 == value)))
        .sortBy(-_._2)
      val greatestOccurence = valueWithOccurences.head._2

      valueWithOccurences.filter(x => x._2 == greatestOccurence)
        .map(_._1)
        .reverse
    }

  }

}
