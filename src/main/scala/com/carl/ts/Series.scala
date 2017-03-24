package com.carl.ts

import java.time.LocalDateTime


object Series {

  /** Create TimeSeries from data in rows format */
  def fromRows(rows: Seq[(LocalDateTime, Double)]): Series = {
    val (idx, vs) = rows.unzip
    new Series(idx, vs)
  }
}

class Series(idx: Seq[LocalDateTime], vs: Seq[Double]) {

  private val index: Seq[LocalDateTime] = idx
  private val values: Seq[Double] = vs

  def length: Int = index.length
}

