package carldata.series

object Metrics {
  /** The root-mean-square deviation (RMSD) or root-mean-square error (RMSE) is a frequently used measure
    * of the differences between values (sample or population values) predicted by a model or an estimator
    * and the values observed.
    * RMSD is always non-negative, and a value of 0 would indicate a perfect fit to the data. In general, a lower RMSD
    * is better than a higher one.
    */
  def rmse(xs: TimeSeries[Double], ys: TimeSeries[Double]): Double = {
    Math.sqrt(mse(xs, ys))
  }

  /** Mean squared error (MSE) of an estimator (of a procedure for estimating an unobserved quantity) measures
    * the average of the squares of the errors—that is, the average squared difference between the estimated
    * values and the actual value.
    */
  def mse(xs: TimeSeries[Double], ys: TimeSeries[Double]): Double = {
    if (xs.isEmpty || ys.isEmpty) 0d
    else {
      val vs = xs.join(ys).values
      val sum: Double = vs.map(v => v._1 - v._2).map(Math.pow(_, 2)).sum
      sum / vs.length
    }
  }

  /** Median absolute error output is non-negative,and a value of 0 would indicate a perfect fit to the data.
    */
  def mae(xs: TimeSeries[Double], ys: TimeSeries[Double]): Double = {
    if (xs.isEmpty || ys.isEmpty) 0d
    else {
      val vs = xs.join(ys).values
        .map(v => v._1 - v._2)
        .map(Math.abs)
        .sorted

      if (vs.isEmpty) Double.NaN
      else if (vs.length % 2 == 0) {
        val (beginning, end) = vs.splitAt(vs.length / 2)
        (beginning.last + end.head) / 2
      }
      else {
        vs.splitAt(vs.length / 2)._2.head
      }
    }
  }

  /** Mean absolute percentage error is a measure of prediction accuracy of a forecasting method.
    */
  def mape(xs: TimeSeries[Double], ys: TimeSeries[Double]): Double = {
    if (xs.isEmpty || ys.isEmpty) 0d
    else {
      val vs = xs.join(ys).values

      (vs.filter(v => v._1 != 0)
        .map(v => (v._1 - v._2) / v._1)
        .map(Math.abs).sum / vs.length) * 100
    }
  }

  /**
    * Mean absolute deviation of a data set is the average of the absolute deviations from a central point.
    */
  def mad(xs: TimeSeries[Double], ys: TimeSeries[Double]): Double = {
    if (xs.isEmpty || ys.isEmpty) 0d
    else {
      val vs = xs.join(ys).values

      vs.map(v => v._1 - v._2)
        .map(Math.abs).sum / vs.length
    }
  }


}
