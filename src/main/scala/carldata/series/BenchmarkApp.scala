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
  def measureGroupBy(ts: TimeSeries[Float]): Unit = ts.groupByTime(_.withMinute(0), _.unzip._2.sum)
  /** Rolling window */
  def measureRollingWindow(ts: TimeSeries[Float]): Unit = ts.rollingWindow(Duration.ofHours(1), _.sum)
  /** Resample */
  def measureResample(ts: TimeSeries[Float]): Unit =  TimeSeries.interpolate(ts, Duration.ofHours(1))
  /** Rolling window */
  def measureIntegrateByTime(ts: TimeSeries[Float]): Unit = TimeSeries.integrateByTime(ts, Duration.ofHours(1))
  /** Rolling window */
  def measureStep(ts: TimeSeries[Float]): Unit = TimeSeries.step(ts, Duration.ofMinutes(1))
  /** Find sessions */
  def measureSessions(ts: TimeSeries[Float]): Unit = Sessions.findSessions(ts, Duration.ofMinutes(10))


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

    println("\n6. Measure: step (This will create 1M series as a output)")
    measure(size1M/5, measureStep)

    println("\n7. Measure: findSessions")
    measure(size1M, measureSessions)

    println()
  }

  private val intFormatter = java.text.NumberFormat.getIntegerInstance

  /** Run several tests anr report result */
  def measure(size: Int, f: TimeSeries[Float] => Unit): Unit = {
    val xs = 1.to(size).toVector
    val ts = TimeSeries.fromTimestamps(xs.map(x => (x.toLong*5*60, x.toFloat)))
    val time: Quantity[Double] = withWarmer(new Warmer.Default).measure {
      f(ts)
    }

    val sizeFormatted = intFormatter.format(size)
    println(s"$sizeFormatted points: $time.")
  }

}