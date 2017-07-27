package carldata.series

import java.time.{LocalDateTime, ZoneOffset}


object TimeSeries {

  /** Create TimeSeries from data in columns format */
  def fromColumns[V: Numeric](index: Seq[LocalDateTime], values: Seq[V]): TimeSeries[V] = {
    new TimeSeries(index.zip(values))
  }

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
class TimeSeries[V: Numeric](idx: Vector[LocalDateTime], ds: Vector[V]) {

  def this(d: Seq[(LocalDateTime, V)]) = {
    this(d.map(_._1).toVector, d.map(_._2).toVector)
  }

  val length: Int = math.min(idx.length, ds.length)
  val index: Vector[LocalDateTime] = idx.take(length)
  val values: Vector[V] = ds.take(length)


  /** Safe get. If element is out of the bounds then 0 is returned */
  def get(i: Int)(implicit num: Numeric[V]): V = values.lift(i).getOrElse(num.zero)

  def head: Option[(LocalDateTime, V)] = {
    for {
      i <- index.headOption
      v <- values.headOption
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

  /** Get slice of series with left side inclusive and right side exclusive
    * this operation is based on index.
    */
  def slice(start: LocalDateTime, end: LocalDateTime): TimeSeries[V] = {
    val d = index.zip(values).filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new TimeSeries(d)
  }

  def differentiate(implicit num: Numeric[V]): TimeSeries[V] = {
    if(index.isEmpty || values.isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.minus(x._2, x._1))
      new TimeSeries(index.tail, vs)(num)
    }
  }

  def integrate(implicit num: Numeric[V]): TimeSeries[V] = {
    if(index.isEmpty || values.isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.plus(x._1, x._2))
      new TimeSeries(index.tail, vs)(num)
    }
  }
}

