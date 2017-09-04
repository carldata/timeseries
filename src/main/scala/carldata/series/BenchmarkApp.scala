package carldata.series

import java.time.Duration

import org.scalameter._



/**
  * Measure library performance
  */
object BenchmarkApp{

  /** map over series */
  def measureMap(ts: TimeSeries[Float]): Unit = ts.map(x => x._2 + 2)
  /** Group by time. 100x slower then map. */
  def measureGroupBy(ts: TimeSeries[Float]): Unit = ts.groupByTime(_.withSecond(0), _.sum)
  /** Rolling window */
  def measureRollingWindow(ts: TimeSeries[Float]): Unit = ts.rollingWindow(Duration.ofMinutes(1), _.sum)
  /** Resample */
  def measureResample(ts: TimeSeries[Float]): Unit =  TimeSeries.resample(ts, Duration.ofMinutes(1))
  /** Rolling window */
  def measureIntegrateByTime(ts: TimeSeries[Float]): Unit = TimeSeries.integrateByTime(ts, Duration.ofMinutes(1))


  /** Run benchmarks */
  def main(args: Array[String]): Unit = {
    val size100K = 100000
    val size1M = 1000000

    println("\n1. Measure: map")
    measure(size1M, measureMap)

    println("\n2. Measure: groupBy")
    measure(size1M, measureGroupBy)

    println("\n3. Measure: rollingWindow")
    measure(size100K, measureRollingWindow)
    measure(size1M, measureRollingWindow)

    println("\n4. Measure: resample")
    measure(size100K, measureResample)
    measure(size1M, measureResample)

    println("\n5. Measure: integrateByTime")
    measure(size1M, measureIntegrateByTime)

    println()
  }

  private val intFormatter = java.text.NumberFormat.getIntegerInstance

  /** Run several tests anr report result */
  def measure(size: Int, f: TimeSeries[Float] => Unit): Unit = {
    val xs = 1.to(size).toVector
    val ts = TimeSeries.fromTimestamps(xs.map(x => (x.toLong*5, x.toFloat)))
    val time: Quantity[Double] = withWarmer(new Warmer.Default).measure {
      f(ts)
    }

    val sizeFormatted = intFormatter.format(size)
    println(s"$sizeFormatted points: $time.")
  }

}