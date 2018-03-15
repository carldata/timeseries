package series

import java.time.{Duration, Instant}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("TimeSeries")
case class JsTimeSeries(idx: js.Array[Int], ds: js.Array[Double]) {

  private def toInt(i: Instant): Int = i.toEpochMilli.toInt

  private def toEpoch(i: Int): Instant = Instant.ofEpochMilli(i)

  private def fromTimeSeries(ts: TimeSeries[Double]): JsTimeSeries = {
    JsTimeSeries(ts.index.map(toInt).toJSArray, ts.values.toJSArray)
  }

  private def toTimeSeries(xs: JsTimeSeries): TimeSeries[Double] = {
    new TimeSeries(xs.index.map(toEpoch).toVector, xs.values.toVector)
  }

  private val timeSeries = new TimeSeries[Double](idx.map(toEpoch).toVector, ds.toVector)

  def this(d: Seq[(Int, Double)]) = {
    this(d.map(_._1).toJSArray, d.map(_._2).toJSArray)
  }

  @JSExport("fromTimestamps")
  def fromTimestamps(rows: Seq[(Int, Double)]): JsTimeSeries = {
    JsTimeSeries(rows.unzip._1.toJSArray, rows.unzip._2.toJSArray)
  }

  @JSExport("empty")
  def empty: JsTimeSeries = new JsTimeSeries(Seq[(Int, Double)]())


  @JSExport("interpolate")
  def interpolate(delta: Int)(implicit num: Fractional[Double]): JsTimeSeries = {
    val ts: TimeSeries[Double] = TimeSeries.interpolate(timeSeries, Duration.ofSeconds(delta))
    fromTimeSeries(ts)
  }

  @JSExport("almostEqual")
  def almostEqual(xs: JsTimeSeries, ys: JsTimeSeries, epsilon: Double): Boolean = {
    TimeSeries.almostEqual(toTimeSeries(xs), toTimeSeries(ys), epsilon)
  }

  @JSExport("differentiate")
  def differentiate(ts: JsTimeSeries): JsTimeSeries = {
    val xs = TimeSeries.differentiate(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("diffOverflow")
  def diffOverflow(ts: JsTimeSeries, overflowValue: Double): JsTimeSeries = {
    val xs = TimeSeries.diffOverflow(toTimeSeries(ts), overflowValue)
    fromTimeSeries(xs)
  }

  @JSExport("diffOverflow")
  def diffOverflow(ts: JsTimeSeries): JsTimeSeries = {
    val xs = TimeSeries.diffOverflow(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("integrate")
  def integrate(ts: JsTimeSeries): JsTimeSeries = {
    val xs = TimeSeries.integrate(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("integrateByTime")
  def integrateByTime(ts: JsTimeSeries, f: Int => Int): JsTimeSeries = {
    def g(i: Instant): Instant = toEpoch(f(toInt(i)))

    val xs = TimeSeries.integrateByTime(toTimeSeries(ts), g)
    fromTimeSeries(xs)
  }

  @JSExport("step")
  def step(ts: JsTimeSeries, delta: Int): JsTimeSeries = {
    val xs = TimeSeries.step(toTimeSeries(ts), Duration.ofSeconds(delta))
    fromTimeSeries(xs)
  }

  @JSExport("length")
  val length: Int = timeSeries.length
  @JSExport("index")
  val index: js.Array[Int] = timeSeries.index.map(toInt).toJSArray
  @JSExport("values")
  val values: js.Array[Double] = timeSeries.values.toJSArray
  @JSExport("dataPoints")
  val dataPoints: js.Array[(Int, Double)] = index.zip(values).toJSArray
  @JSExport("resolution")
  val resolution: Duration = timeSeries.resolution

  @JSExport("isEmpty")
  def isEmpty: Boolean = timeSeries.isEmpty

  @JSExport("nonEmpty")
  def nonEmpty: Boolean = timeSeries.nonEmpty

  @JSExport("get")
  def get(i: Int): Double ={
    timeSeries.get(i)
  }

  @JSExport("head")
  def head: Option[(Int, Double)] = timeSeries.head.map(x => (toInt(x._1), x._2))

  @JSExport("last")
  def last: Option[(Int, Double)] = timeSeries.last.map(x => (toInt(x._1), x._2))

  @JSExport("sortByIndex")
  def sortByIndex: JsTimeSeries = {
    val ts = timeSeries.sortByIndex
    fromTimeSeries(ts)
  }

  @JSExport("filter")
  def filter(f: ((Int, Double)) => Boolean): JsTimeSeries = {
    def g(x: (Instant, Double)): Boolean = f(toInt(x._1), x._2)

    val ts = timeSeries.filter(g)
    fromTimeSeries(ts)
  }

  @JSExport("map")
  def map(f: ((Int, Double)) => Double): JsTimeSeries = {
    def g(x: (Instant, Double)): Double = f(toInt(x._1), x._2)

    val ts = timeSeries.map(g)
    fromTimeSeries(ts)
  }

  @JSExport("mapValues")
  def mapValues(f: Double => Double): JsTimeSeries = {
    val ts = timeSeries.mapValues(f)
    JsTimeSeries(ts.index.map(toInt).toJSArray, ts.values.toJSArray)
  }

  @JSExport("merge")
  def merge(ts: JsTimeSeries): JsTimeSeries = {
    val xs = timeSeries.merge(TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector))
    fromTimeSeries(xs)
  }

  @JSExport("slice")
  def slice(start: Int, end: Int): JsTimeSeries = {
    val ts = timeSeries.slice(toEpoch(start), toEpoch(end))
    fromTimeSeries(ts)
  }

  @JSExport("take")
  def take(n: Int): JsTimeSeries = JsTimeSeries(idx.take(n), values.take(n))

  @JSExport("groupByTime")
  def groupByTime(g: Instant => Instant, f: Seq[(Instant, Double)] => Double): JsTimeSeries = {
    val ts = timeSeries.groupByTime(g, f)
    fromTimeSeries(ts)
  }

  @JSExport("resample")
  def resample(delta: Int, f: ((Int, Double), (Int, Double), Int) => Double): JsTimeSeries = {
    def g(x1: (Instant, Double), x2: (Instant, Double), tsh: Instant) =
      f((toInt(x1._1), x1._2), (toInt(x2._1), x2._2), toInt(tsh))

    val ts = timeSeries.resample(Duration.ofSeconds(delta), g)
    fromTimeSeries(ts)
  }

  @JSExport("resampleWithDefault")
  def resampleWithDefault(delta: Int, default: Double)(implicit num: Fractional[Double]): JsTimeSeries = {
    val ts = timeSeries.resampleWithDefault(Duration.ofSeconds(delta), default)
    fromTimeSeries(ts)
  }

  @JSExport("addMissing")
  def addMissing(delta: Int, f: ((Int, Double), (Int, Double), Int) => Double): JsTimeSeries = {
    def g(x1: (Instant, Double), x2: (Instant, Double), tsh: Instant) =
      f((toInt(x1._1), x1._2), (toInt(x2._1), x2._2), toInt(tsh))

    val ts = timeSeries.addMissing(Duration.ofSeconds(delta), g)
    fromTimeSeries(ts)
  }

  @JSExport("rollingWindow")
  def rollingWindow(windowSize: Duration, f: Seq[Double] => Double): JsTimeSeries = {
    val ts = timeSeries.rollingWindow(windowSize, f)
    fromTimeSeries(ts)
  }

  @JSExport("shiftTime")
  def shiftTime(delta: Int): JsTimeSeries = JsTimeSeries(index.map(x => x + delta), values)

}




