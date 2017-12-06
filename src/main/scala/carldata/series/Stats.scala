package carldata.series

/** Calculate Time Series statistics */
object Stats {

  case class MeanAndVariance(mean: Double, variance: Double)

  def meanAndVariance[V: Fractional](values: Seq[V])(implicit num: Fractional[V]): MeanAndVariance = {
    if (values.isEmpty) MeanAndVariance(0, 0)
    else {
      val m = num.toDouble(values.sum(num)) / values.length
      val s = values
        .map(num.toDouble)
        .map(x => Math.pow(x - m, 2))
        .sum
      MeanAndVariance(m, s / values.length)
    }
  }

  /** Calculate covariance between 2 places in the series */
  def autoCovariance[V: Fractional](values: Seq[V], pos1: Int, pos2: Int, size: Int, mean: Double)
                                   (implicit num: Fractional[V]): Double = {
    val maxSize = if (pos2 > pos1) Math.min(values.length - pos2, size) else Math.min(values.length - pos1, size)
    val vs = 0.until(maxSize)
      .map(i => (num.toDouble(values(pos1 + i)), num.toDouble(values(pos2 + i))))
      .map(x => (x._1 - mean) * (x._2 - mean))

    if (vs.isEmpty) 0
    else vs.sum / vs.length
  }

  /** Calculate correlation between 2 places in the series */
  def autoCorrelation[V: Fractional](values: Seq[V], pos1: Int, pos2: Int, size: Int): Double = {
    val MeanAndVariance(mean, variance) = meanAndVariance(values)
    if (variance == 0) 0
    else autoCovariance(values, pos1, pos2, size, mean) / variance
  }

  /** Calculate covariance with two-pass algorithm. */
  def covariance[V: Fractional](xs: Seq[V], ys: Seq[V])(implicit num: Fractional[V]): Double = {
    if (xs.isEmpty || ys.isEmpty) 0
    else {
      val xs2 = xs.map(num.toDouble)
      val ys2 = ys.map(num.toDouble)

      val mean1 = xs2.sum / xs.length
      val mean2 = ys2.sum / ys.length
      val vs = xs2.zip(ys2).map(x => (x._1 - mean1) * (x._2 - mean2))
      vs.sum / xs2.length
    }
  }
}
