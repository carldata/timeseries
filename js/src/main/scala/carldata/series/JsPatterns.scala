package carldata.series

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Patterns")
class JsPatterns {
  @JSExport("daily")
  def daily(xs: JsTimeSeries): JsTimeSeries = {
    val ts = Patterns.daily(xs.toTimeSeries)
    xs.fromTimeSeries(ts)
  }
}
