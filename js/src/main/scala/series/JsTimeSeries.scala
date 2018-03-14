package series

import java.time.{Duration, Instant}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("TimeSeries")
case class JsTimeSeries[V](idx: js.Array[Int], ds: js.Array[V]) {

  private def toInt(i: Instant): Int = i.toEpochMilli.toInt

  private def toEpoch(i: Int): Instant = Instant.ofEpochMilli(i)

  private def fromTimeSeries[V: Numeric](ts: TimeSeries[V]): JsTimeSeries[V] = JsTimeSeries(ts.index.map(toInt).toJSArray, ts.values.toJSArray)

  private def toTimeSeries[V: Numeric](xs: JsTimeSeries[V]): TimeSeries[V] = new TimeSeries(xs.index.map(toEpoch).toVector, xs.values.toVector)

  val timeSeries = new TimeSeries[V](idx.map(toEpoch).toVector, ds.toVector)

  def this(d: Seq[(Int, V)]) = {
    this(d.map(_._1).toJSArray, d.map(_._2).toJSArray)
  }

  @JSExport("fromTimestamps")
  def fromTimestamps[V: Numeric](rows: Seq[(Int, V)]): JsTimeSeries[V] = {
    JsTimeSeries(rows.unzip._1.toJSArray, rows.unzip._2.toJSArray)
  }

  @JSExport("empty")
  def empty: JsTimeSeries[V] = new JsTimeSeries[V](Seq[(Int, V)]())


  @JSExport("interpolate")
  def interpolate[V: Fractional](xs: JsTimeSeries[V], delta: Duration): JsTimeSeries[V] = {
    val ts: TimeSeries[V] = TimeSeries.interpolate(toTimeSeries(xs), delta)
    fromTimeSeries(ts)
  }

  @JSExport("almostEqual")
  def almostEqual[V: Numeric](xs: JsTimeSeries[V], ys: JsTimeSeries[V], epsilon: V): Boolean = {
    TimeSeries.almostEqual(toTimeSeries(xs), toTimeSeries(ys), epsilon)
  }

  @JSExport("differentiate")
  def differentiate[V: Numeric](ts: JsTimeSeries[V]): JsTimeSeries[V] = {
    val xs = TimeSeries.differentiate(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("diffOverflow")
  def diffOverflow[V: Numeric](ts: JsTimeSeries[V], overflowValue: V): JsTimeSeries[V] = {
    val xs = TimeSeries.diffOverflow(toTimeSeries(ts), overflowValue)
    fromTimeSeries(xs)
  }

  @JSExport("diffOverflow")
  def diffOverflow[V: Numeric](ts: JsTimeSeries[V]): JsTimeSeries[V] = {
    val xs = TimeSeries.diffOverflow(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("integrate")
  def integrate[V: Numeric](ts: JsTimeSeries[V]): JsTimeSeries[V] = {
    val xs = TimeSeries.integrate(toTimeSeries(ts))
    fromTimeSeries(xs)
  }

  @JSExport("integrateByTime")
  def integrateByTime[V: Numeric](ts: JsTimeSeries[V], f: Int => Int): JsTimeSeries[V] = {
    def g(i: Instant): Instant = toEpoch(f(toInt(i)))

    val xs = TimeSeries.integrateByTime(toTimeSeries(ts), g)
    fromTimeSeries(xs)
  }

  @JSExport("step")
  def step[V: Fractional](ts: JsTimeSeries[V], d: Duration): JsTimeSeries[V] = {
    val xs = TimeSeries.step(toTimeSeries(ts), d)
    fromTimeSeries(xs)
  }

  @JSExport("length")
  val length: Int = timeSeries.length
  @JSExport("index")
  val index: js.Array[Int] = timeSeries.index.map(toInt).toJSArray
  @JSExport("values")
  val values: js.Array[V] = timeSeries.values.toJSArray
  @JSExport("dataPoints")
  val dataPoints: js.Array[(Int, V)] = index.zip(values).toJSArray
  @JSExport("resolution")
  val resolution: Duration = timeSeries.resolution

  @JSExport("isEmpty")
  def isEmpty: Boolean = timeSeries.isEmpty

  @JSExport("nonEmpty")
  def nonEmpty: Boolean = timeSeries.nonEmpty

  @JSExport("get")
  def get(i: Int)(implicit num: Numeric[V]): V = timeSeries.get(i)

  @JSExport("head")
  def head: Option[(Int, V)] = timeSeries.head.map(x => (toInt(x._1), x._2))

  @JSExport("last")
  def last: Option[(Int, V)] = timeSeries.last.map(x => (toInt(x._1), x._2))

  @JSExport("sortByIndex")
  def sortByIndex(implicit num: Numeric[V]): JsTimeSeries[V] = {
    val ts = timeSeries.sortByIndex
    fromTimeSeries(ts)
  }

  @JSExport("filter")
  def filter(f: ((Int, V)) => Boolean)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    def g(x: (Instant, V)): Boolean = f(toInt(x._1), x._2)

    val ts = timeSeries.filter(g)
    fromTimeSeries(ts)
  }

  @JSExport("map")
  def map(f: ((Int, V)) => V)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    def g(x: (Instant, V)): V = f(toInt(x._1), x._2)

    val ts = timeSeries.map(g)
    fromTimeSeries(ts)
  }

  @JSExport("mapValues")
  def mapValues[U: Numeric](f: V => U): JsTimeSeries[U] = {
    val ts = timeSeries.mapValues(f)
    JsTimeSeries(ts.index.map(toInt).toJSArray, ts.values.toJSArray)
  }

  @JSExport("merge")
  def merge(ts: JsTimeSeries[V])(implicit num: Numeric[V]): JsTimeSeries[V] = {
    val xs = timeSeries.merge(TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector))
    fromTimeSeries(xs)
  }

  @JSExport("slice")
  def slice(start: Int, end: Int)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    val ts = timeSeries.slice(toEpoch(start), toEpoch(end))
    fromTimeSeries(ts)
  }

  @JSExport("take")
  def take(n: Int): JsTimeSeries[V] = JsTimeSeries(idx.take(n), values.take(n))

  @JSExport("groupByTime")
  def groupByTime(g: Instant => Instant, f: Seq[(Instant, V)] => V)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    val ts = timeSeries.groupByTime(g, f)
    fromTimeSeries(ts)
  }

  @JSExport("resample")
  def resample(delta: Duration, f: ((Int, V), (Int, V), Int) => V)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    def g(x1: (Instant, V), x2: (Instant, V), tsh: Instant) =
      f((toInt(x1._1), x1._2), (toInt(x2._1), x2._2), toInt(tsh))

    val ts = timeSeries.resample(delta, g)
    fromTimeSeries(ts)
  }

  @JSExport("resampleWithDefault")
  def resampleWithDefault(delta: Duration, default: V)(implicit num: Fractional[V]): JsTimeSeries[V] = {
    val ts = timeSeries.resampleWithDefault(delta, default)
    fromTimeSeries(ts)
  }

  @JSExport("addMissing")
  def addMissing(delta: Duration, f: ((Int, V), (Int, V), Int) => V)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    def g(x1: (Instant, V), x2: (Instant, V), tsh: Instant) =
      f((toInt(x1._1), x1._2), (toInt(x2._1), x2._2), toInt(tsh))

    val ts = timeSeries.addMissing(delta, g)
    fromTimeSeries(ts)
  }

  @JSExport("rollingWindow")
  def rollingWindow(windowSize: Duration, f: Seq[V] => V)(implicit num: Numeric[V]): JsTimeSeries[V] = {
    val ts = timeSeries.rollingWindow(windowSize, f)
    fromTimeSeries(ts)
  }

  @JSExport("shiftTime")
  def shiftTime(d: Duration): JsTimeSeries[V] = {
    JsTimeSeries(index.map(x => x + d.toMillis.toInt), values)
  }

  @JSExport("join")
  def join[U](ts: JsTimeSeries[U])(implicit num: Numeric[(V, U)]): JsTimeSeries[(V, U)] = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val ys = timeSeries.join(xs)
    JsTimeSeries(ys.index.map(toInt).toJSArray, ys.values.toJSArray)
  }

  @JSExport("joinLeft")
  def joinLeft[U](ts: JsTimeSeries[U], default: U)(implicit num: Numeric[(V, U)]): JsTimeSeries[(V, U)] = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val ys = timeSeries.joinLeft(xs, default)
    JsTimeSeries(ys.index.map(toInt).toJSArray, ys.values.toJSArray)
  }

  @JSExport("joinOuter")
  def joinOuter[U](ts: JsTimeSeries[U], defaultLeft: V, defaultRight: U)(implicit num: Numeric[(V, U)]): JsTimeSeries[(V, U)] = {
    val xs = TimeSeries(ts.index.map(toEpoch).toVector, ts.values.toVector)
    val ys = timeSeries.joinOuter(xs, defaultLeft, defaultRight)
    JsTimeSeries(ys.index.map(toInt).toJSArray, ys.values.toJSArray)
  }
}

