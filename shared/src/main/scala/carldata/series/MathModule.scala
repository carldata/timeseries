package carldata.series

object MathModule {

  implicit class TimeSeriesMathOperations[V](ts: TimeSeries[V]) {

    def add(v: V)(implicit num: Numeric[V]): TimeSeries[V] = {
      ts.mapValues(x => num.plus(x, v))
    }

  }

}
