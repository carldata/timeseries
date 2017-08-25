package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}


object TimeSeries {

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): TimeSeries[V] = {
    new TimeSeries(rows.map(r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }

  /** Create empty series */
  def empty[V: Numeric]: TimeSeries[V] = {
    new TimeSeries[V](Seq[(LocalDateTime, V)]())
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

  /** Aggregate date by time */
  def groupByTime(g: LocalDateTime => LocalDateTime, f: Seq[V] => V): TimeSeries[V] = {
    val gs = index.zip(values)
      .groupBy(x => g(x._1))
      .mapValues(xs => f(xs.map(_._2)))
      .toSeq
      .sortWith((x, y) => x._1.isBefore(y._1))

    new TimeSeries(gs)
  }

  def rollingWindow(windowSize: Duration, stepSize: Duration, f: Seq[V] => V): TimeSeries[V] = {
    val rs = Iterator.iterate(index.head)(_.plus(stepSize))
      .takeWhile(x => x.isBefore(index.last)).toSeq
      .map { s => {
        val first = index.zip(values).minBy(x => x._1.isBefore(s) || x._1.isEqual(s))
        val window = slice(first._1, first._1.plus(windowSize))
        (window.index.last, f(window.values))
      }
      }

    new TimeSeries(rs)
  }
}

