package carldata.series

import org.scalameter._
import org.scalatest.{FlatSpec, Matchers}


/**
  * Measure library performance
  */
class Benchmarks extends FlatSpec with Matchers {

  "Benchmarks" should "measure 1M points" in {
    val size1M = 1000000
    measureMap(size1M)
//    measureGroupByTime(size1M)
  }

  "Benchmarks" should "measure 100K points" in {
    val size100K = 100000
    measureGroupByTime(size100K)
  }

  /** map over series */
  def measureMap(size: Int): Unit = {
    val xs = 1.to(size).toVector
    val ts = TimeSeries.fromTimestamps(xs.map(x => (x.toLong, x.toFloat)))

    val time: Quantity[Double] = withWarmer(new Warmer.Default).measure {
      ts.map(x => x._2 + 2)
    }
    println(s"map over $size points: $time.")
  }

  /** Group by time. 100x slower then map. */
  def measureGroupByTime(size: Int): Unit = {
    val xs = 1.to(size).toVector
    val ts = TimeSeries.fromTimestamps(xs.map(x => (x.toLong * 1000, x.toFloat)))

    val time: Quantity[Double] = withWarmer(new Warmer.Default).measure {
      ts.groupByTime(_.withSecond(0), _.sum)
    }
    println(s"GroupByTime over $size points: $time")
  }

}