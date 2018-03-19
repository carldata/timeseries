package carldata.series

object Outliers {

  /** Remove outliers */
  def remove[V: Numeric](xs: TimeSeries[V], min: V, max: V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val ys = xs.index.zip(xs.values).flatMap(x => {
      if (num.compare(x._2, min) < 0) None
      else if (num.compare(x._2, max) > 0) None
      else Some(x)
    }).unzip

    TimeSeries(ys._1, ys._2)
  }

  /** Remove outliers by set min/max values on their place */
  def trim[V: Numeric](xs: TimeSeries[V], min: V, max: V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val vs = xs.values.map(x => {
      if (num.compare(x, min) < 0) min
      else if (num.compare(x, max) > 0) max
      else x
    })
    TimeSeries(xs.idx, vs)
  }

  /** Remove outliers by interpolate values on their place */
  def interpolate[V: Numeric](xs: TimeSeries[V], min: V, max: V, f: (V, V) => V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val zipped = (num.zero +: xs.values).zip(xs.values).zip(xs.values.tail :+ xs.values.last)
    val vs = zipped.map(x => {
      if (num.compare(x._1._2, min) < 0) f(x._1._1, x._2)
      else if (num.compare(x._1._2, max) > 0) f(x._1._1, x._2)
      else x._1._2
    })
    TimeSeries[V](xs.idx, vs)
  }
}
