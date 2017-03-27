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
}

class Series[V: Numeric](d: Seq[(LocalDateTime, V)]) {

  val data: Vector[(LocalDateTime, V)] = d.toVector

  val index: Seq[LocalDateTime] = data.map(_._1)

  val values: Seq[V] = data.map(_._2)

  def length: Int = data.length

  def get(i: Int)(implicit num: Numeric[V]): V = data.lift(i).map(_._2).getOrElse(num.zero)

  def max: V = values.max

  def min: V = values.min

  def sum(implicit num: Numeric[V]): V = values.fold(num.zero)(num.plus)

  def map(f: ((LocalDateTime, V)) => V): Series[V] = {
    val xs: Seq[V] = data.map(f)
    new Series(index.zip(xs))
  }

  def fold(z: V)(f: (V, V) => V): V = {
    values.fold(z)(f)
  }

  /** Get slice of series with left side inclusive and right side exclusive */
  def slice(start: LocalDateTime, end: LocalDateTime): Series[V] = {
    val d = data.filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new Series(d)
  }

  def differentiate(implicit num: Numeric[V]): Series[V] = {
    val vs: Seq[V] = values.zip(values.tail).map(x => num.minus(x._2, x._1))
    new Series(index.tail.zip(vs))(num)
  }

  def integrate(implicit num: Numeric[V]): Series[V] = {
    val vs: Seq[V] = values.zip(values.tail).map(x => num.plus(x._1, x._2))
    new Series(index.tail.zip(vs))(num)
  }
}

