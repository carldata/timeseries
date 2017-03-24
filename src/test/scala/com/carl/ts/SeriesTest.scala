package com.carl.ts

import org.scalatest._


class SeriesTest extends FlatSpec with Matchers {

  "Series" should "have length equal to its index" in {
    val series = new Series(Seq("2015", "2016", "2017"), Seq(1.2))
    series.length shouldBe 3
  }

  it should "be build from the rows" in {
    val series = Series.fromRows(Seq(("2015", 1), ("2016", 3.4), ("2017", 5.6)))
    series.length shouldBe 3
  }
}