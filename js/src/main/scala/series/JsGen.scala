package series

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("Gen")
object JsGen {
  private val jts = JsTimeSeries(js.Array(), js.Array())

  @JSExport("mkIndex")
  def mkIndex(start: Int, end: Int, duration: Int): Vector[Int] = {
    val xs = ListBuffer[Int]()

    @tailrec def appendR(pos: Int): Unit = {
      if (pos <= end) {
        xs += pos
        appendR(pos + duration)
      }
    }

    appendR(start)
    xs.toVector
  }

  @JSExport("constant")
  def constant(idx: Vector[Int], v: Double): JsTimeSeries = {
    val ts = Gen.constant(idx.map(jts.toEpoch), v)
    jts.fromTimeSeries(ts)
  }

  @JSExport("repeat")
  def repeat(pattern: JsTimeSeries, start: Int, end: Int): JsTimeSeries = {
    val ts = Gen.repeat(pattern.toTimeSeries, jts.toEpoch(start), jts.toEpoch(end))
    pattern.fromTimeSeries(ts)
  }

  @JSExport("randomNoise")
  def randomNoise(idx: Vector[Int], mean: Double, variance: Double): JsTimeSeries = {
    val ts = Gen.randomNoise(idx.map(jts.toEpoch), mean, variance)
    jts.fromTimeSeries(ts)
  }

  @JSExport("randomWalk")
  def randomWalk(idx: Vector[Int]): JsTimeSeries = {
    val ts = Gen.randomWalk(idx.map(jts.toEpoch))
    jts.fromTimeSeries(ts)
  }

}
