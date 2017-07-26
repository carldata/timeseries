package com.carl.ts

import java.time.{LocalDateTime, ZoneOffset}


object Series {

  /** Create TimeSeries from data in columns format */
  def fromColumns[V: Numeric](index: Seq[LocalDateTime], values: Seq[V]): Series[V] = {
    new Series(index.zip(values))
  }

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): Series[V] = {
    new Series(rows.map( r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }

  /** Create empty series */
  def empty[V: Numeric]: Series[V] = {
    new Series[V](Seq[(LocalDateTime, V)]())
  }
}

/**
  * TimeSeries contains data indexed by DateTime. The type of stored data
  * is parametric.
  */
case class Series[V: Numeric](index: Vector[LocalDateTime], values: Vector[V]) {

  def this(d: Seq[(LocalDateTime, V)]) = {
    this(d.map(_._1).toVector, d.map(_._2).toVector)
  }

  def length: Int = index.length

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
  def filter(f: ((LocalDateTime, V)) => Boolean): Series[V] = {
    new Series(index.zip(values).filter(f))
  }

  /** Map by index and value. Create new values */
  def map(f: ((LocalDateTime, V)) => V): Series[V] = {
    val vs: Vector[V] = index.zip(values).map(f)
    new Series(index, vs)
  }

  /** Get slice of series with left side inclusive and right side exclusive
    * this operation is based on index.
    */
  def slice(start: LocalDateTime, end: LocalDateTime): Series[V] = {
    val d = index.zip(values).filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new Series(d)
  }

  def differentiate(implicit num: Numeric[V]): Series[V] = {
    if(values.isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.minus(x._2, x._1))
      new Series(index, vs)(num)
    }
  }

  def integrate(implicit num: Numeric[V]): Series[V] = {
    if(values.isEmpty) {
      this
    } else {
      val vs: Vector[V] = values.zip(values.tail).map(x => num.plus(x._1, x._2))
      new Series(index.tail, vs)(num)
    }
  }
}

