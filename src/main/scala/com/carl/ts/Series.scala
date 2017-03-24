package com.carl.ts

object Series {

  /** Create TimeSeries from data in rows format */
  def fromRows(rows: Seq[(String, Double)]): Series = {
    val (idx, vs) = rows.unzip
    new Series(idx, vs)
  }
}

class Series(idx: Seq[String], vs: Seq[Double]) {

  private val index: Seq[String] = idx
  private val values: Seq[Double] = vs

  def length: Int = index.length
}

