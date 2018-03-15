package series

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Outliers")
class JsOutliers {
  @JSExport("remove")
  def remove(xs: JsTimeSeries, min: Double, max: Double): JsTimeSeries = {
    val ts = Outliers.remove(xs.toTimeSeries, min, max)
    xs.fromTimeSeries(ts)
  }

  @JSExport("trim")
  def trim(xs: JsTimeSeries, min: Double, max: Double): JsTimeSeries = {
    val ts = Outliers.trim(xs.toTimeSeries, min, max)
    xs.fromTimeSeries(ts)
  }

  @JSExport("interpolate")
  def interpolate(xs: JsTimeSeries, min: Double, max: Double, f: (Double, Double) => Double): JsTimeSeries = {
    val ts = Outliers.interpolate(xs.toTimeSeries, min, max, f)
    xs.fromTimeSeries(ts)
  }
}
