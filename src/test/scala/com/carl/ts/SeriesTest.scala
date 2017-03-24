package com.carl.ts

import java.time.LocalDateTime

import org.scalatest._


class SeriesTest extends FlatSpec with Matchers {

  "Series" should "have length equal to its index" in {
    val now = LocalDateTime.now()
    val series = new Series(
      Seq(now, now.plusMinutes(1), now.plusMinutes(2)),
      Seq(1.2))
    series.length shouldBe 3
  }

  it should "be build from the rows" in {
    val now = LocalDateTime.now()
    val series = Series.fromRows(Seq((now, 1), (now.plusMinutes(1), 3.4), (now.plusMinutes(2), 5.6)))
    series.length shouldBe 3
  }

  it should "be build from timestamps" in {
    val series = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.length shouldBe 3
  }

  it should "access element at given index" in {
    val series = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(2) shouldBe 5.6
  }

  it should "return 0 for element outside of the index" in {
    val series = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(20) shouldBe 0
  }

  it should "return minimum value" in {
    val series = Series.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.min() shouldBe -3.4
  }

  it should "return maximum value" in {
    val series = Series.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.max() shouldBe 5.6
  }

  //  it "map over series" $ do
//  let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
//  let ys = fmap (+ 2) xs
//    TS.values ys `shouldBe` [12.0, 3.2, 34.4, 2.65, 13.0]

}