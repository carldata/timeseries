package carldata.extras

import carldata.series.TimeSeries

object MathModule {

  implicit class MathTimeSeriesOperations[V](ts: TimeSeries[V]) {
    /**
      * Add constant for each element of series
      */
    def add(v: V)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.mapValues(x => num.plus(x, v))
    }

    /**
      * Subtract constant from each element of series
      */
    def subtract(v: V)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.mapValues(x => num.minus(x, v))
    }

    /**
      * Each element of series multiply by constant
      */
    def multiply(v: V)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.mapValues(x => num.times(x, v))
    }

    /**
      * Each element of series divide by constant
      */
    def divide(v: V)(implicit num: Fractional[V]): TimeSeries[V] = {
      ts.mapValues(x => num.div(x, v))
    }
  }

}
