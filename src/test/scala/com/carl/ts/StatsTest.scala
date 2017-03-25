package com.carl.ts

import com.carl.ts.Stats._
import org.scalatest._



class StatsTest extends FlatSpec with Matchers {

  "Stats" should "calculate mean" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    series.mean shouldEqual 4
  }

  it should "calculate stddev" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 8)))
    series.stddev - 3.78594 < 0.0001 shouldBe true
  }

}