package com.carl.ts

import org.scalatest._


class SeriesTest extends FlatSpec with Matchers {

  "Series" should "have length equal to its index" in {
    val series = new Series(Seq("2015", "2016", "2017"), Seq(1.2, 3.4, 5.6))
    series.length shouldBe 3
  }

//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }
}