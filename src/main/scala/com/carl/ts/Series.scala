package com.carl.ts

import java.time.{LocalDateTime, ZoneOffset}


object Series {

  /** Create TimeSeries from data in rows format */
  def fromRows(rows: Seq[(LocalDateTime, Double)]): Series = {
    val (idx, vs) = rows.unzip
    new Series(idx, vs)
  }

  /** Create TimeSeries from timestamps */
  def fromTimestamps(rows: Seq[(Long, Double)]): Series = {
    val (idx, vs) = rows.unzip
    val idx2 = idx.map( i => LocalDateTime.ofEpochSecond(i, 0, ZoneOffset.UTC))
    new Series(idx2, vs)
  }
}

class Series(idx: Seq[LocalDateTime], vs: Seq[Double]) {

  private val index: Seq[LocalDateTime] = idx
  private val values: Vector[Double] = vs.toVector

  def length: Int = index.length

  def get(i: Int): Double = if(i >= 0 && i < values.length) { values(i) } else { 0 }
}

