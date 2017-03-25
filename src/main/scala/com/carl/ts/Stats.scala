package com.carl.ts

/**
  * Created by Krzysztof Langner on 2017-03-25.
  */
object Stats {

  trait StatsLike {
    def mean(xs: Seq[Double]): Double
    def variance(xs: Seq[Double]): Double
    def stddev(xs: Seq[Double]): Double
  }
}
