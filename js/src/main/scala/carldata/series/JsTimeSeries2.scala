package carldata.series

import java.time.{Duration, Instant}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.JSConverters._

@JSExportTopLevel("TimeSeries2")
object JsTimeSeries2 {
  private def toEpoch(i: Int): Instant = Instant.ofEpochMilli(i)

  private def toInt(i: Instant): Int = i.toEpochMilli.toInt

  @JSExport("join")
  def join(ts: JsTimeSeries, ts2: JsTimeSeries): JsTimeSeries2 = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val xs2 = TimeSeries(ts2.index.map(toEpoch).toVector, ts2.values.toVector)
    val ys = xs.join(xs2)
    JsTimeSeries2(ys.index.map(JsTimeSeries2.toInt).toJSArray, ys.values.toJSArray)
  }

  @JSExport("joinLeft")
  def joinLeft(ts: JsTimeSeries, ts2: JsTimeSeries, default: Double): JsTimeSeries2 = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val xs2 = TimeSeries(ts2.index.map(toEpoch).toVector, ts2.values.toVector)
    val ys = xs.joinLeft(xs2, default)
    JsTimeSeries2(ys.index.map(JsTimeSeries2.toInt).toJSArray, ys.values.toJSArray)
  }

  @JSExport("joinOuter")
  def joinOuter(ts: JsTimeSeries, ts2: JsTimeSeries, defaultLeft: Double, defaultRight: Double): JsTimeSeries2 = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val xs2 = TimeSeries(ts2.index.map(toEpoch).toVector, ts2.values.toVector)
    val ys = xs.joinOuter(xs2, defaultLeft, defaultRight)
    JsTimeSeries2(ys.index.map(JsTimeSeries2.toInt).toJSArray, ys.values.toJSArray)
  }
}

case class JsTimeSeries2(idx: js.Array[Int], ds: js.Array[(Double, Double)]) {
  private val timeSeries = new TimeSeries[(Double, Double)](idx.map(JsTimeSeries2.toEpoch).toVector, ds.toVector)

  def this(d: Seq[(Int, (Double, Double))]) = {
    this(d.map(_._1).toJSArray, d.map(_._2).toJSArray)
  }

  def fromTimeSeries(ts: TimeSeries[(Double, Double)]): JsTimeSeries2 = {
    JsTimeSeries2(ts.index.map(JsTimeSeries2.toInt).toJSArray, ts.values.toJSArray)
  }

  private def toTimeSeries(xs: JsTimeSeries2): TimeSeries[(Double, Double)] = {
    new TimeSeries(xs.index.map(JsTimeSeries2.toEpoch).toVector, xs.values.toVector)
  }

  @JSExport("length")
  val length: Int = timeSeries.length
  @JSExport("index")
  val index: js.Array[Int] = timeSeries.index.map(JsTimeSeries2.toInt).toJSArray
  @JSExport("values")
  val values: js.Array[(Double, Double)] = timeSeries.values.toJSArray
  @JSExport("dataPoints")
  val dataPoints: js.Array[(Int, (Double, Double))] = index.zip(values).toJSArray
  @JSExport("resolution")
  val resolution: Duration = timeSeries.resolution

  @JSExport("isEmpty")
  def isEmpty: Boolean = timeSeries.isEmpty

  @JSExport("nonEmpty")
  def nonEmpty: Boolean = timeSeries.nonEmpty

  @JSExport("get")
  def get(i: Int): (Double, Double) = dataPoints.filter(x => x._1 == i).head._2

  @JSExport("head")
  def head: Option[(Int, (Double, Double))] = timeSeries.head.map(x => (JsTimeSeries2.toInt(x._1), x._2))

  @JSExport("last")
  def last: Option[(Int, (Double, Double))] = timeSeries.last.map(x => (JsTimeSeries2.toInt(x._1), x._2))

  @JSExport("sortByIndex")
  def sortByIndex: JsTimeSeries2 = {
    val ts = timeSeries.sortByIndex
    fromTimeSeries(ts)
  }

  @JSExport("filter")
  def filter(f: ((Int, (Double, Double))) => Boolean): JsTimeSeries2 = {
    def g(x: (Instant, (Double, Double))): Boolean = f(JsTimeSeries2.toInt(x._1), x._2)

    val ts = timeSeries.filter(g)
    fromTimeSeries(ts)
  }

  @JSExport("map")
  def map(f: ((Int, (Double, Double))) => (Double, Double)): JsTimeSeries2 = {
    def g(x: (Instant, (Double, Double))): (Double, Double) = f(JsTimeSeries2.toInt(x._1), x._2)

    val ts = timeSeries.map(g)
    fromTimeSeries(ts)
  }

  @JSExport("mapValues")
  def mapValues(f: (Double, Double) => (Double, Double)): JsTimeSeries2 = {
    val vs: Vector[(Double, Double)] = values.toVector.map(x => f(x._1,x._2))
    val ts = TimeSeries(index.toVector.map(JsTimeSeries2.toEpoch), vs)
    fromTimeSeries(ts)
  }

  @JSExport("merge")
  def merge(ts: JsTimeSeries2): JsTimeSeries2 = {
    val xs = timeSeries.merge(TimeSeries(ts.index.map(JsTimeSeries2.toEpoch).toVector, ts.values.toVector))
    fromTimeSeries(xs)
  }

  @JSExport("slice")
  def slice(start: Int, end: Int): JsTimeSeries2 = {
    val ts = timeSeries.slice(JsTimeSeries2.toEpoch(start), JsTimeSeries2.toEpoch(end))
    fromTimeSeries(ts)
  }

  @JSExport("take")
  def take(n: Int): JsTimeSeries2 = JsTimeSeries2(idx.take(n), values.take(n))

  @JSExport("groupByTime")
  def groupByTime(g: Instant => Instant, f: Seq[(Instant, (Double, Double))] => (Double, Double)): JsTimeSeries2 = {
    val ts = timeSeries.groupByTime(g, f)
    fromTimeSeries(ts)
  }

  //@JSExport("resample")
  //def resample(delta: Int, f: ((Int, (Double, Double)), (Int, (Double, Double)), Int) => (Double, Double)): JsTimeSeries2
  //not implemented

  @JSExport("resampleWithDefault")
  def resampleWithDefault(delta: Int, default: (Double, Double))(implicit num: Fractional[(Double, Double)]): JsTimeSeries2 = {
    val ts = timeSeries.resampleWithDefault(Duration.ofSeconds(delta), default)
    fromTimeSeries(ts)
  }

  @JSExport("addMissing")
  def addMissing(delta: Int, f: ((Int, (Double, Double)), (Int, (Double, Double)), Int) => (Double, Double)): JsTimeSeries2 = {
    def g(x1: (Instant, (Double, Double)), x2: (Instant, (Double, Double)), tsh: Instant) =
      f((JsTimeSeries2.toInt(x1._1), x1._2), (JsTimeSeries2.toInt(x2._1), x2._2), JsTimeSeries2.toInt(tsh))

    val ts = timeSeries.addMissing(Duration.ofSeconds(delta), g)
    fromTimeSeries(ts)
  }

  @JSExport("rollingWindow")
  def rollingWindow(windowSize: Duration, f: Seq[(Double, Double)] => (Double, Double)): JsTimeSeries2 = {
    val ts = timeSeries.rollingWindow(windowSize, f)
    fromTimeSeries(ts)
  }

  @JSExport("shiftTime")
  def shiftTime(delta: Int): JsTimeSeries2 = JsTimeSeries2(index.map(x => x + delta), values)

}
