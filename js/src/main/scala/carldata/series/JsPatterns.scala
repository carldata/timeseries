package carldata.series

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Patterns")
class JsPatterns {
  @JSExport("daily")
  def daily(xs: JsTimeSeries): JsTimeSeries2 = {
    val ts = Patterns.daily(xs.toTimeSeries)
    new JsTimeSeries2(Seq()).fromTimeSeries(ts)
  }
}
