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
    val mean = meanAndVariance(values).mean
    val c1 = autoCovariance(values, pos1, pos2, size, mean)
    val c2 = autoCovariance(values, pos1, pos1, size, mean)
    if (c2 == 0) 0.0 else c1 / c2
  }

  /** Calculate covariance with two-pass algorithm. */
  def covariance[V: Fractional](xs: Seq[V], ys: Seq[V])(implicit num: Fractional[V]): V = {
    def sumFractional(vs: Seq[V]): V = {
      if (vs.nonEmpty) {
        val x = sumFractional(vs.tail)
        num.plus(vs.head, x)
      }
      else num.zero
    }

    xs.foreach(println)
    ys.foreach(println)
    val length = num.fromInt(xs.length)
    val mean1: V = num.div(sumFractional(xs), length)
    val mean2: V = num.div(sumFractional(ys), length)
    val vs = xs.zip(ys)
      .map { x =>
        num.times(num.minus(x._1, mean1), num.minus(x._2, mean2))
      }
    println(mean1)
    num.div(sumFractional(vs), length)
  }
}
