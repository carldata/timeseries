package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object TimeSeries {

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): TimeSeries[V] = {
    new TimeSeries(rows.map(r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }

  /** Create empty series */
  def empty[V: Numeric]: TimeSeries[V] = {
    new TimeSeries[V](Seq[(LocalDateTime, V)]())
  }

  def resample[V: Fractional](ts: TimeSeries[V], delta: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    def internal_resample(inputTs: TimeSeries[V], partial: TimeSeries[V]): TimeSeries[V] = {
      inputTs.head match {
        case Some(x) =>
          val ts = new TimeSeries(inputTs.index.tail, inputTs.values.tail)(num)
          val y = partial.last.get
          val nidx = y._1.plus(delta)
          val tx = num.fromInt(Duration.between(nidx, x._1).toMillis.toInt)
          val ty = num.fromInt(Duration.between(y._1, nidx).toMillis.toInt)

          val mu = num.plus(num.times(num.div(ty, num.plus(tx, ty)), x._2), num.times(num.div(tx, num.plus(tx, ty)), y._2))
          val out = if (nidx.isBefore(x._1)) internal_resample(inputTs, new TimeSeries(partial.index :+ nidx, partial.values :+ mu)(num))
          else if (nidx.isEqual(x._1)) internal_resample(ts, new TimeSeries(partial.index :+ x._1, partial.values :+ x._2)(num))
          else internal_resample(ts, partial)
          out
        case _ => {
          partial
        }
      }

    }

    val res = internal_resample(new TimeSeries(ts.index, ts.values)(num), new TimeSeries(Vector(ts.index.head), Vector(ts.values.head))(num))
    new TimeSeries(res.index, res.values)(num)
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
    if(isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.minus(x._2, x._1))
      new TimeSeries(index.tail, vs)(num)
    }
  }

  /** Accumulate sum for each point */
  def integrate(implicit num: Numeric[V]): TimeSeries[V] = {
    if(isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.plus(x._1, x._2))
      new TimeSeries(index.tail, vs)(num)
    }
  }

  /** Aggregate date by time */
  def groupByTime(g: LocalDateTime => LocalDateTime, f: Seq[V] => V): TimeSeries[V] = {
    if(isEmpty) this
    else {
      val xs = mutable.ListBuffer[(LocalDateTime, ArrayBuffer[V])]((g(index.head), mutable.ArrayBuffer()))
      for ((k,v) <- index.zip(values)) {
        val last = xs.last
        val t = g(k)
        if(last._1.isEqual(t)) last._2 += v
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

