package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


object TimeSeries {

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): TimeSeries[V] = {
    new TimeSeries(rows.map(r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }

  /** Create empty series */
  def empty[V: Numeric]: TimeSeries[V] = {
    new TimeSeries[V](Seq[(LocalDateTime, V)]())
  }

  /**
    * Resample given TimeSeries with indexes separated by delta
    */
  def resample[V: Fractional](xs: TimeSeries[V], delta: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    val ys: mutable.ListBuffer[V] = ListBuffer()
    val ts = Iterator.iterate(xs.index.head)(_.plusNanos(delta.toNanos))
      .takeWhile(_.isBefore(xs.index.last.plusNanos(1))).toVector

    def g(ts: Vector[LocalDateTime], xs: Vector[LocalDateTime], vs: Vector[V], prev: V): TimeSeries[V] = {
      val tsh = ts.head
      val xsh = xs.head
      if (xsh.isEqual(tsh)) {
        ys.append(vs.head)
        if (ts.size == 1) TimeSeries(ts, ys.toVector)(num) else g(ts.tail, xs.tail, vs.tail, vs.head)
      }
      else if (tsh.isBefore(xsh)) {
        val ysh = if (ts.size != 1) ts.tail.head else xsh
        val tx = num.fromInt(Duration.between(tsh, xsh).toMillis.toInt)
        val ty = num.fromInt(Duration.between(tsh, ysh).toMillis.toInt)
        val mu = num.plus(num.times(num.div(ty, num.plus(tx, ty)), vs.head), num.times(num.div(tx, num.plus(tx, ty)), prev))
        ys.append(mu)
        if (ts.size == 1) TimeSeries(ts, ys.toVector)(num) else g(ts.tail, xs, vs, mu)
      }
      else {
        g(ts, xs.tail, vs.tail, vs.head)
      }
    }

    g(ts, xs.index, xs.values, num.fromInt(0))
    TimeSeries(ts, ys.toVector)(num)
  }

}

/**
  * TimeSeries contains data indexed by DateTime. The type of stored data
  * is parametric.
  */
case class TimeSeries[V: math.Numeric](idx: Vector[LocalDateTime], ds: Vector[V]) {

  def this(d: Seq[(LocalDateTime, V)]) = {
    this(d.map(_._1).toVector, d.map(_._2).toVector)
  }

  val length: Int = math.min(idx.length, ds.length)
  val index: Vector[LocalDateTime] = idx.take(length)
  val values: Vector[V] = ds.take(length)

  /** Check is series is empty */
  def isEmpty: Boolean = index.isEmpty || values.isEmpty

  /** Safe get. If element is out of the bounds then 0 is returned */
  def get(i: Int)(implicit num: Numeric[V]): V = values.lift(i).getOrElse(num.zero)

  def head: Option[(LocalDateTime, V)] = {
    for {
      i <- index.headOption
      v <- values.headOption
    } yield (i, v)
  }

  /** Get last element of the series */
  def last: Option[(LocalDateTime, V)] = {
    for {
      i <- index.lastOption
      v <- values.lastOption
    } yield (i, v)
  }

  def max: V = values.max

  def min: V = values.min

  def sum(implicit num: Numeric[V]): V = values.fold(num.zero)(num.plus)

  /** Filter by index and value */
  def filter(f: ((LocalDateTime, V)) => Boolean): TimeSeries[V] = {
    new TimeSeries(index.zip(values).filter(f))
  }

  /** Map by index and value. Create new values */
  def map(f: ((LocalDateTime, V)) => V): TimeSeries[V] = {
    val vs: Vector[V] = index.zip(values).map(f)
    new TimeSeries(index, vs)
  }

  /** Map over values. */
  def mapValues(f: V => V): TimeSeries[V] = {
    val vs: Vector[V] = values.map(f)
    new TimeSeries(index, vs)
  }

  /** Get slice of series with left side inclusive and right side exclusive
    * this operation is based on index.
    */
  def slice(start: LocalDateTime, end: LocalDateTime): TimeSeries[V] = {
    val d = index.zip(values).filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new TimeSeries(d)
  }

  /** Return new series with difference between 2 points */
  def differentiate(implicit num: Numeric[V]): TimeSeries[V] = {
    if (isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.minus(x._2, x._1))
      new TimeSeries(index.tail, vs)(num)
    }
  }

  /** Accumulate sum for each point */
  def integrate(implicit num: Numeric[V]): TimeSeries[V] = {
    if (isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.plus(x._1, x._2))
      new TimeSeries(index.tail, vs)(num)
    }
  }

  /**
    * Integrate series for selected window.
    * Windows are not overlapping and sum starts at 0 at the beginning of each window
    */
  def integrateByTime(windowSize: Duration)(implicit num: Numeric[V]): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val end = index.head.plus(windowSize)
      // This buffer could be used inside foldLeft, but idea will show wrong errors in += operation then
      val xs = ArrayBuffer.empty[V]
      index.zip(values).foldLeft[(V, LocalDateTime)]((num.zero, end)) { (acc, x) =>
        if (x._1.isBefore(acc._2)) {
          val v = num.plus(acc._1, x._2)
          xs += v
          (v, acc._2)
        } else {
          xs += x._2
          (x._2, acc._2.plus(windowSize))
        }
      }

      TimeSeries(index, xs.toVector)(num)
    }
  }

  /** Aggregate date by time */
  def groupByTime(g: LocalDateTime => LocalDateTime, f: Seq[V] => V): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val xs = ListBuffer[(LocalDateTime, ArrayBuffer[V])]((g(index.head), ArrayBuffer()))
      for ((k, v) <- index.zip(values)) {
        val last = xs.last
        val t = g(k)
        if (last._1.isEqual(t)) last._2 += v
        else xs += ((t, ArrayBuffer(v)))
      }
      TimeSeries(xs.map(_._1).toVector, xs.map(x => f(x._2)).toVector)
    }
  }

  /** Rolling window operation */
  def rollingWindow(windowSize: Duration, f: Seq[V] => V): TimeSeries[V] = {
    val rs = index.map { t =>
      val window = slice(t.minus(windowSize), t.plusNanos(1))
      f(window.values)
    }

    new TimeSeries(index, rs)
  }
}

