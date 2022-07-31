package carldata.series

/** Calculate Time Series statistics */
object Stats {

  case class MeanAndVariance(mean: Double, variance: Double)

  def meanAndVariance[V: Fractional](values: Seq[V], sample: Boolean = false)(implicit num: Fractional[V]): MeanAndVariance = {
    if (values.isEmpty) MeanAndVariance(0, 0)
    else {
      val m = num.toDouble(values.sum(num)) / values.length
      val s = values
        .map(num.toDouble)
        .map(x => Math.pow(x - m, 2))
        .sum
      if (sample) MeanAndVariance(m, s / (values.length - 1))
      else MeanAndVariance(m , s / values.length)
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

  /** Calculate correlation between 2 series */
  def correlation[V: Fractional](xs: Seq[V], ys: Seq[V])(implicit num: Fractional[V]): Double = {
    val xs2 = xs.map(num.toDouble)
    val ys2 = ys.map(num.toDouble)
    val MeanAndVariance(mean1, variance1) = meanAndVariance(xs2)
    val MeanAndVariance(mean2, variance2) = meanAndVariance(ys2)

    if (variance1 * variance2 == 0) 0
    else {
      val vs = xs2.zip(ys2).map(x => (x._1 - mean1) * (x._2 - mean2))
      vs.sum / (variance1 * variance2)
    }
  }

  /** Normalize series by mean and standard deviation */
  def normalize[V: Fractional](xs: Seq[V])(implicit num: Fractional[V]): Seq[Double] = {
    val xs2 = xs.map(num.toDouble)
    val MeanAndVariance(mean, variance) = meanAndVariance(xs2)
    val std = Math.sqrt(variance)
    xs2.map(x => (x - mean) / std)
  }

  /** Find percentile
    *
    * @param p desired percentile
    * */
  def percentile[V: Fractional](xs: Seq[V], p: Int)(implicit num: Fractional[V]): Double = {
    val xs2 = xs.map(num.toDouble).sorted
    val pos = (p * (xs.length + 1)).toDouble / 100
    if (pos < 1) xs2.head
    else if (pos >= xs.length) xs2.last
    else {
      val pf = Math.floor(pos).toInt
      val lower = xs2(pf - 1)
      val upper = xs2(pf)
      val d = pos - Math.floor(pos)
      lower + d * (upper - lower)
    }
  }

  /** This is the 50th percentile **/
  def median[V: Fractional](xs: Seq[V]): Double = percentile(xs, 50)

}
