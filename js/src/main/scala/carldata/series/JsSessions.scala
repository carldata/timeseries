package carldata.series

import java.time.Duration

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Sessions")
class JsSessions {

  case class JsSession(startIndex: Int, endIndex: Int)

  private val jts = JsTimeSeries(js.Array(), js.Array())

  @JSExport("findSessions")
  def findSessions(ts: JsTimeSeries): Seq[JsSession] = {
    Sessions.findSessions(ts.toTimeSeries)
      .map(x => JsSession(jts.toInt(x.startIndex), jts.toInt(x.endIndex)))
  }
  
  @JSExport("findSessions")
  def findSessions(ts: JsTimeSeries, tolerance: Int): Seq[JsSession] = {
    val t = Duration.ofSeconds(tolerance)
    Sessions.findSessions(ts.toTimeSeries, t)
      .map(x => JsSession(jts.toInt(x.startIndex), jts.toInt(x.endIndex)))
  }

}
