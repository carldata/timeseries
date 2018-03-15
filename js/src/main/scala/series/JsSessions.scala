package series

import java.time.Duration

import series.Sessions.Session

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Sessions")
class JsSessions {

  @JSExport("findSessions")
  def findSessions(ts: JsTimeSeries): Seq[Session] = Sessions.findSessions(ts.toTimeSeries)

  @JSExport("findSessions")
  def findSessions(ts: JsTimeSeries, tolerance: Int): Seq[Session] = {
    val t = Duration.ofSeconds(tolerance)
    Sessions.findSessions(ts.toTimeSeries, t)
  }

}
