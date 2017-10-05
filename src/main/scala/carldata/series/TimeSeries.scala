package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import carldata.series.TimeSeries.reindex

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


object TimeSeries {

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): TimeSeries[V] = {
    new TimeSeries(rows.map(r => (LocalDateTime.ofEpochSecond(r._1, 0, ZoneOffset.UTC), r._2)))
  }

  /** Create empty series */
  def empty[V: Numeric]: TimeSeries[V] = {
    new TimeSeries[V](Seq[(LocalDateTime, V)]())
  }

  /** Return new series by interpolate missing points */
  def interpolate[V: Numeric](xs: TimeSeries[V], delta: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    def f(x1: (LocalDateTime, V), x2: (LocalDateTime, V), tsh: LocalDateTime) = {
      val tx = num.fromInt(Duration.between(tsh, x1._1).toMillis.toInt)
      val ty = num.fromInt(Duration.between(tsh, x2._1).toMillis.toInt)
      num.plus(num.times(num.div(ty, num.plus(tx, ty)), x1._2), num.times(num.div(tx, num.plus(tx, ty)), x2._2))
    }

    xs.resample(delta, f)
  }

  /** Return new series with difference between 2 points */
  def differentiate[V: Numeric](ts: TimeSeries[V])(implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val vs: Vector[V] = ts.values.zip(ts.values.tail).map(x => num.minus(x._2, x._1))
      TimeSeries(ts.index.tail, vs)
    }
  }

  /**
    * Return new series with difference between 2 points with the possibility of overflow value
    * Use case:
    * Older sensor equipment often used simple counters to record events.
    * These counters would continue to some max value (i.e. 100) and then rollover to 0
    */
  def diffOverflow[V: Numeric](ts: TimeSeries[V], overflowValue: V)(implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val vs: Vector[V] = ts.values.zip(ts.values.tail).map { x =>
        if (num.lt(x._2, x._1)) num.minus(num.plus(x._2, overflowValue), x._1)
        else num.minus(x._2, x._1)
      }
      TimeSeries(ts.index.tail, vs)
    }
  }

  /** Accumulate sum for each point */
  def integrate[V: Numeric](ts: TimeSeries[V])(implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val vs: Vector[V] = ts.values.zip(ts.values.tail).map(x => num.plus(x._1, x._2))
      new TimeSeries(ts.index.tail, vs)
    }
  }

  /**
    * Integrate series for selected window.
    * Windows are not overlapping and sum starts at 0 at the beginning of each window
    */
  def integrateByTime[V: Numeric](ts: TimeSeries[V], g: LocalDateTime => LocalDateTime)
                                 (implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val startTime = g(ts.index.head)
      // This buffer could be used inside foldLeft, but then Intellij Idea will show wrong errors in += operation.
      val xs = ListBuffer.empty[V]
      ts.dataPoints.foldLeft[(LocalDateTime, V)]((startTime, num.zero)) { (acc, x) =>
        val t = g(x._1)
        if (t.isEqual(acc._1)) {
          val v = num.plus(acc._2, x._2)
          xs += v
          (acc._1, v)
        } else {
          xs += x._2
          (t, x._2)
        }
      }
      TimeSeries(ts.index, xs.toVector)
    }
  }

  /** Create new index with the step equal to duration, for a given time range */
  private def reindex(start: LocalDateTime, end: LocalDateTime, delta: Duration): Vector[LocalDateTime] = {
    Iterator.iterate(start)(_.plusNanos(delta.toNanos)).takeWhile(_.isBefore(end)).toVector
  }

  /**
    * This function resamples index. But the values are not interpolated but just fractions
    * of current values.
    * For example if we have value 10 every hour and we run step with 15 minutes range
    * we will get values 2.5 every 15 minutes (10/4)
    */
  def step[V: Fractional](ts: TimeSeries[V], d: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    val index = reindex(ts.index.head, ts.index.last, d)
    val builder: mutable.ListBuffer[V] = ListBuffer()

    @tailrec def stepR(dp: Vector[(LocalDateTime, V)], newIdx: Vector[LocalDateTime]): Vector[V] = {
      if (dp.isEmpty) builder.toVector
      else {
        val p = dp.head
        val xs = newIdx.takeWhile(_.isBefore(p._1))
        val len = xs.length
        if (len > 0) {
          val v = num.div(p._2, num.fromInt(len))
          0.until(len).foreach(_ => builder.append(v))
        }
        stepR(dp.tail, newIdx.drop(len))
      }
    }

    val vs = stepR(ts.dataPoints, index)
    TimeSeries(index.take(vs.length), vs)
  }

}

/**
  * TimeSeries contains data indexed by DateTime. The type of stored data
  * is parametric.
  */
case class TimeSeries[V](idx: Vector[LocalDateTime], ds: Vector[V]) {

  def this(d: Seq[(LocalDateTime, V)]) = {
    this(d.map(_._1).toVector, d.map(_._2).toVector)
  }

  val length: Int = math.min(idx.length, ds.length)
  val index: Vector[LocalDateTime] = idx.take(length)
  val values: Vector[V] = ds.take(length)
  val dataPoints: Vector[(LocalDateTime, V)] = index.zip(values)

  /** Check is series is empty */
  def isEmpty: Boolean = index.isEmpty || values.isEmpty

  /** Check is series is non empty */
  def nonEmpty: Boolean = !isEmpty

  /** Safe get. If element is out of the bounds then 0 is returned */
  def get(i: Int)(implicit num: Numeric[V]): V = values.lift(i).getOrElse(num.zero)

  def head: Option[(LocalDateTime, V)] = {
    for {
      i <- index.headOption
      v <- values.headOption
    } yield (i, v)
  }

  /** Get last element of the series */
  def last: Option[(LocalDateTime, V)] = {
    for {
      i <- index.lastOption
      v <- values.lastOption
    } yield (i, v)
  }

  /** Filter by index and value */
  def filter(f: ((LocalDateTime, V)) => Boolean): TimeSeries[V] = {
    new TimeSeries(index.zip(values).filter(f))
  }

  /** Map by index and value. Create new values */
  def map(f: ((LocalDateTime, V)) => V): TimeSeries[V] = {
    val vs: Vector[V] = index.zip(values).map(f)
    new TimeSeries(index, vs)
  }

  /** Map over values. */
  def mapValues[U](f: V => U): TimeSeries[U] = {
    val vs: Vector[U] = values.map(f)
    new TimeSeries(index, vs)
  }

  /** Get slice of series with left side inclusive and right side exclusive
    * this operation is based on index.
    */
  def slice(start: LocalDateTime, end: LocalDateTime): TimeSeries[V] = {
    val d = index.zip(values).filter(x => (x._1.isAfter(start) || x._1.isEqual(start)) && x._1.isBefore(end))
    new TimeSeries(d)
  }

  /**
    * Aggregate data points.
    *
    * @param g This function transforms current data point time into new time. All points with the same
    *          time will be integrated into single point
    * @param f This function defines how to aggregate points with the same transformed time
    */
  def groupByTime(g: LocalDateTime => LocalDateTime, f: Seq[(LocalDateTime, V)] => V): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val xs = ListBuffer[(LocalDateTime, ArrayBuffer[(LocalDateTime, V)])]((g(index.head), ArrayBuffer()))
      for ((k, v) <- index.zip(values)) {
        val last = xs.last
        val t = g(k)
        if (last._1.isEqual(t)) last._2 += ((k, v))
        else xs += ((t, ArrayBuffer((k, v))))
      }
      TimeSeries(xs.map(_._1).toVector, xs.map(x => f(x._2)).toVector)
    }
  }

  /**
    * Resample given TimeSeries with indexes separated by delta.
    * This function will ensure that series is evenly spaced.
    * @param delta  Distance between points
    * @param f      Function which approximates missing points
    */
  def resample(delta: Duration, f: ((LocalDateTime, V), (LocalDateTime, V), LocalDateTime) => V)(implicit num: Numeric[V]): TimeSeries[V] = {
    if (index.isEmpty) TimeSeries.empty[V](num)
    else {
      val ys: mutable.ListBuffer[V] = ListBuffer()
      val ts = Iterator.iterate(index.head)(_.plusNanos(delta.toNanos))
        .takeWhile(_.isBefore(index.last.plusNanos(1))).toVector

      @tailrec def g(ts: Vector[LocalDateTime], xs: Vector[LocalDateTime], vs: Vector[V], prev: V): Unit = {
        val tsh = ts.head
        val xsh = xs.head
        if (xsh.isEqual(tsh)) {
          ys.append(vs.head)
          if (ts.size != 1) g(ts.tail, xs.tail, vs.tail, vs.head)
        }
        else if (tsh.isBefore(xsh)) {
          val ysh = if (ts.size != 1) ts.tail.head else xsh
          val mu = f((xsh, vs.head), (ysh, prev), tsh)
          ys.append(mu)
          if (ts.size != 1) g(ts.tail, xs, vs, mu)
        }
        else {
          g(ts, xs.tail, vs.tail, vs.head)
        }
      }

      g(ts, index, values, num.fromInt(0))
      TimeSeries(ts, ys.toVector)
    }
  }

  /**
    * Resample series. If there are any missing points then they will be replaced by given default value
    */
  def resampleWithDefault(delta: Duration, default: V)(implicit num: Fractional[V]): TimeSeries[V] = {
    def f(x1: (LocalDateTime, V), x2: (LocalDateTime, V), tsh: LocalDateTime) = default

    resample(delta, f)
  }

  /**
    * Add missing points to the time series. The output series will have all its own points
    * and some new points if there are missing at every duration.
    * @param delta  Expected distance between points
    * @param f      This function will approximate missing points.
    */
  def addMissing(delta: Duration, f: ((LocalDateTime, V), (LocalDateTime, V), LocalDateTime) => V): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val ys: mutable.ListBuffer[(LocalDateTime, V)] = ListBuffer()
      val resampledIndex = reindex(index.head, index.last, delta)

      @tailrec def g(ts: Vector[LocalDateTime], xs: Vector[(LocalDateTime, V)], prev: (LocalDateTime, V)): Unit = {
        if (xs.nonEmpty) {
          val xsh = xs.head
          if (ts.isEmpty) {
            ys.append(xs.head)
            g(ts, xs.tail, xsh)

          } else if (xsh._1.isEqual(ts.head)) {
            ys.append(xs.head)
            g(ts.tail, xs.tail, xsh)
          }
          else if (ts.head.isBefore(xsh._1)) {
            val mu = f(prev, xsh, ts.head)
            val dp = (ts.head, mu)
            ys.append(dp)
            g(ts.tail, xs, dp)
          }
          else {
            ys.append(xs.head)
            g(ts, xs.tail, xsh)
          }
        }
      }

      val dp = dataPoints
      g(resampledIndex, dp, dp.head)
      new TimeSeries(ys)
    }
  }

  /** Rolling window operation */
  def rollingWindow(windowSize: Duration, f: Seq[V] => V): TimeSeries[V] = {

    @tailrec def g(v: Vector[(LocalDateTime, V)], out: Vector[V]): Vector[V] = {
      if (v.nonEmpty) {
        val splitIndex: Int = v.indexWhere(x => x._1.isBefore(v.head._1.minus(windowSize).minusNanos(1)))
        val window = if (splitIndex > 0) v.take(splitIndex) else v
        g(v.tail, out :+ f(window.map(x => x._2)))
      }
      else out
    }

    new TimeSeries(index, g(index.zip(values).reverse, Vector.empty).reverse)
  }

  /** Repeat series */
  def repeat(start: LocalDateTime, end: LocalDateTime, d: Duration): TimeSeries[V] = {
    val ts = slice(start, start.plus(d))
    if (ts.isEmpty) ts
    else {
      @tailrec def repeatR(offset: Duration, dp: (Vector[LocalDateTime], Vector[V])): (Vector[LocalDateTime], Vector[V]) = {
        val idx = ts.index.map(_.plus(offset))
        if (idx.head.isBefore(end)) repeatR(offset.plus(d), (dp._1 ++ idx, dp._2 ++ ts.values))
        else dp
      }

      val (idx, vs) = repeatR(Duration.ZERO, (Vector(), Vector()))
      TimeSeries(idx, vs)
    }
  }

  /** Shift index by specific time duration */
  def shiftTime(d: Duration, forward: Boolean): TimeSeries[V] = {
    val idx = index.map(i => if (forward) i.plus(d) else i.minus(d))
    TimeSeries(idx, values)
  }

  /** Remove outliers */
  def removeOutliers(min: V, max: V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val ys = index.zip(values).flatMap(x => {
      if (num.compare(x._2, min) < 0) None
      else if (num.compare(x._2, max) > 0) None
      else Some(x)
    }).unzip

    TimeSeries(ys._1, ys._2)
  }

  /** Remove outliers by set min/max values on their place */
  def trimOutliers(min: V, max: V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val vs = values.map(x => {
      if (num.compare(x, min) < 0) min
      else if (num.compare(x, max) > 0) max
      else x
    })
    TimeSeries(idx, vs)
  }

  /** Remove outliers by interpolate values on their place */
  def interpolateOutliers(min: V, max: V, f: (V, V) => V)(implicit num: Numeric[V]): TimeSeries[V] = {
    val zipped = (num.zero +: values).zip(values).zip(values.tail :+ values.last)
    val vs = zipped.map(x => {
      if (num.compare(x._1._2, min) < 0) f(x._1._1, x._2)
      else if (num.compare(x._1._2, max) > 0) f(x._1._1, x._2)
      else x._1._2
    })
    TimeSeries(idx, vs)
  }

  /** Inner join. Only include points which have the same data in both series */
  def join[U](ts: TimeSeries[U]): TimeSeries[(V, U)] = {
    val builder: mutable.ListBuffer[(LocalDateTime, (V, U))] = ListBuffer()

    @tailrec def joinR(dp1: Vector[(LocalDateTime, V)],
                       dp2: Vector[(LocalDateTime, U)]): Seq[(LocalDateTime, (V, U))] = {
      if (dp1.isEmpty | dp2.isEmpty) builder
      else {
        val p1 = dp1.head
        val p2 = dp2.head
        if (p1._1.isEqual(p2._1)) {
          builder.append((p1._1, (p1._2, p2._2)))
          joinR(dp1.tail, dp2.tail)
        }
        else if (p1._1.isBefore(p2._1)) joinR(dp1.tail, dp2)
        else joinR(dp1, dp2.tail)

      }
    }

    new TimeSeries(joinR(dataPoints, ts.dataPoints))
  }

  /** Left join. If right series doesn't have a data point then put default value. */
  def joinLeft[U](ts: TimeSeries[U], default: U): TimeSeries[(V, U)] = {
    val builder: mutable.ListBuffer[(LocalDateTime, (V, U))] = ListBuffer()

    @tailrec def joinR(dp1: Vector[(LocalDateTime, V)],
                       dp2: Vector[(LocalDateTime, U)]): Seq[(LocalDateTime, (V, U))] = {
      if (dp1.isEmpty | dp2.isEmpty) builder
      else {
        val p1 = dp1.head
        val p2 = dp2.head
        if (p1._1.isEqual(p2._1)) {
          builder.append((p1._1, (p1._2, p2._2)))
          joinR(dp1.tail, dp2.tail)
        } else if (p1._1.isBefore(p2._1)) {
          builder.append((p1._1, (p1._2, default)))
          joinR(dp1.tail, dp2)
        } else joinR(dp1, dp2.tail)

      }
    }

    new TimeSeries(joinR(dataPoints, ts.dataPoints))
  }

}

