package carldata.series

import carldata.series.Stats.MeanAndVariance

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Stats")
object JsStats {

  @JSExport("meanAndVariance")
  def meanAndVariance(values: js.Array[Double], sample: Boolean = false): MeanAndVariance = Stats.meanAndVariance(values.toSeq)

  @JSExport("autoCovariance")
  def autoCovariance(values: js.Array[Double], pos1: Int, pos2: Int, size: Int, mean: Double): Double =
    Stats.autoCovariance(values.toSeq, pos1, pos2, size, mean)

  @JSExport("autoCorrelation")
  def autoCorrelation(values: js.Array[Double], pos1: Int, pos2: Int, size: Int): Double =
    Stats.autoCorrelation(values.toSeq, pos1, pos2, size)

  @JSExport("covariance")
  def covariance(xs: js.Array[Double], ys: js.Array[Double]): Double = Stats.covariance(xs.toSeq, ys.toSeq)

  @JSExport("correlation")
  def correlation(xs: js.Array[Double], ys: js.Array[Double]): Double = Stats.correlation(xs.toSeq, ys.toSeq)

  @JSExport("normalize")
  def normalize(xs: js.Array[Double]): js.Array[Double] = Stats.normalize(xs.toSeq).toJSArray

  @JSExport("percentile")
  def percentile(xs: js.Array[Double], p: Int): Double = Stats.percentile(xs.toSeq, p)

  @JSExport("median")
  def median(xs: js.Array[Double]): Double = percentile(xs, 50)

}
