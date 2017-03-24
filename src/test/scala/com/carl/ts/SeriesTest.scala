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

//  it "element by index" $ do
//  let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
//  fmap TS.dpValue (TS.elemAt 2 xs) `shouldBe` Just 32.4
//
//  it "map over series" $ do
//  let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
//  let ys = fmap (+ 2) xs
//    TS.values ys `shouldBe` [12.0, 3.2, 34.4, 2.65, 13.0]
//
//  it "maximum value" $ do
//  let xs = TS.tsSeries [1..] [10.0, 1.2, 32.4, 0.65, 11.0]
//  maximum xs `shouldBe` 32.4
//
//  it "minimum value" $ do
//  let xs = TS.tsSeries [1..5] [10.0, 1.2, 32.4, 0.65, 11.0]
//  minimum xs `shouldBe` 0.65
}