package com.carl.ts

/**
  * Created by Krzysztof Langner on 2017-03-24.
  */
class Series(idx: Seq[String], vs: Seq[Double]) {

  private val index: Seq[String] = idx
  private val values: Seq[Double] = vs

  def length: Int = index.length
}
