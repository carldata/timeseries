package com.carl.ts

import java.time.{LocalDateTime, ZoneOffset}

import org.scalatest._


class SeriesTest extends FlatSpec with Matchers {

  "Series" should "have length equal to its index" in {
    val now = LocalDateTime.now()
    val series = new Series(Seq((now,1), (now.plusMinutes(1), 2), (now.plusMinutes(2), 3)))
    series.length shouldBe 3
  }

  it should "be build from the rows" in {
    val now = LocalDateTime.now()
    val series = Series.fromColumns(Seq(now, now.plusMinutes(1), now.plusMinutes(2)), Seq(1, 2, 3, 4, 5.6))
    series.length shouldBe 3
  }

  it should "be build from timestamps" in {
    val series: Series[Double] = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.length shouldBe 3
  }

  it should "access element at given index" in {
    val series: Series[Double] = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(2) shouldBe 5.6
  }

  it should "return 0 for element outside of the index" in {
    val series: Series[Double] = Series.fromTimestamps(Seq((1, 1), (2, 3.4), (3, 5.6)))
    series.get(20) shouldBe 0
  }

  it should "return minimum value" in {
    val series: Series[Double] = Series.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.min shouldBe -3.4
  }

  it should "return maximum value" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.max shouldBe 5
  }

  it should "sum its elements" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 5)))
    series.sum shouldBe 3
  }

  it should "map over its values" in {
    val series: Series[Double] = Series.fromTimestamps(Seq((1, 1), (2, -3.4), (3, 5.6)))
    series.map(x => x._2 + 2).max shouldBe 7.6
  }

  it should "fold values" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 6)))
    series.fold(0)((x, y) => x+y) shouldBe 4
  }

  it should "return subseries" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 1), (2, -3), (3, 6), (4, 6), (5, 6), (6, 6)))
    val start = LocalDateTime.ofEpochSecond(2, 0, ZoneOffset.UTC)
    val end = LocalDateTime.ofEpochSecond(6, 0, ZoneOffset.UTC)
    series.slice(start, end).length shouldBe 4
  }

  it should "differentiate" in {
    val series: Series[Int] = Series.fromTimestamps(Seq((1, 2), (2, -4), (3, -6), (4, 8)))
    val expected: Series[Int] = Series.fromTimestamps(Seq((2, -6), (3, -2), (4, 14)))
    series.differentiate.values shouldBe expected.values
  }
}