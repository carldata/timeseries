package com.carl.ts

import java.time.{LocalDateTime, ZoneOffset}


object Series {

  /** Create TimeSeries from data in columns format */
  def fromColumns(index: Seq[LocalDateTime], values: Seq[Double]): Series = {
    new Series(index.zip(values))
  }

  /** Create TimeSeries from timestamps */
  def fromTimestamps(rows: Seq[(Long, Double)]): Series = {
    new Series(rows.map( r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }
}

class Series(d: Seq[(LocalDateTime, Double)]) {

  val data: Vector[(LocalDateTime, Double)] = d.toVector

  val index: Seq[LocalDateTime] = data.map(_._1)

  val values: Seq[Double] = data.map(_._2)

  def length: Int = data.length

  def get(i: Int): Double = data.lift(i).map(_._2).getOrElse(0)

  def max(): Double = values.max

  def min(): Double = values.min

  def map(f: ((LocalDateTime, Double)) => Double): Series = {
    val xs: Seq[Double] = data.map(f)
    new Series(index.zip(xs))
  }

  def fold(z: Double)(f: (Double, Double) => Double): Double = {
    values.fold(z)(f)
  }

  /** Get slice of series with left side inclusive and right side exclusive */
  def slice(start: LocalDateTime, end: LocalDateTime): Series = {
    val d = data.filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new Series(d)
  }
}

