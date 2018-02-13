package carldata.series

import java.time.{Duration, Instant}

import carldata.series.TimeSeries.mkIndex

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}


object TimeSeries {

  /** Create TimeSeries from timestamps */
  def fromTimestamps[V: Numeric](rows: Seq[(Long, V)]): TimeSeries[V] = {
    new TimeSeries(rows.map(r => (Instant.ofEpochSecond(r._1), r._2)))
  }

  /** Create empty series */
  def empty[V]: TimeSeries[V] = {
    new TimeSeries[V](Seq[(Instant, V)]())
  }

  /** Return new series by interpolate missing points */
  def interpolate[V: Numeric](xs: TimeSeries[V], delta: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant) = {
      val tx = num.fromInt(Duration.between(tsh, x1._1).toMillis.toInt)
      val ty = num.fromInt(Duration.between(tsh, x2._1).toMillis.toInt)
      num.plus(num.times(num.div(ty, num.plus(tx, ty)), x1._2), num.times(num.div(tx, num.plus(tx, ty)), x2._2))
    }

    xs.resample(delta, f)
  }

  /**
    * Return true 2 series have the same index and the difference between 2 values,
    * (with the same index) is equal or less than epsilon
    */
  def almostEqual[V: Numeric](xs: TimeSeries[V], ys: TimeSeries[V], epsilon: V)(implicit num: Numeric[V]): Boolean = {
    def diff(x: (V, V)): V = num.abs(num.minus(x._1, x._2))

    def compare(x: V): Boolean = num.compare(num.minus(epsilon, x), num.zero) >= 0

    if (xs.index == ys.index) {
      xs.values.zip(ys.values)
        .map(diff)
        .map(compare)
        .forall(identity)
    } else false
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

  /**
    * Predictive overflow based on the assumption that
    * if the next value is smaller then the current value, then overflow happend
    */
  def diffOverflow[V: Numeric](ts: TimeSeries[V])(implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val vs: Vector[V] = ts.values.zip(ts.values.tail).map { x =>
        if (num.lt(x._2, x._1)) x._2
        else if (num.gt(x._2, x._1)) num.minus(x._2, x._1)
        else num.minus(x._2, x._1)
      }
      TimeSeries(ts.index, ts.values.head +: vs)
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
  def integrateByTime[V: Numeric](ts: TimeSeries[V], g: Instant => Instant)
                                 (implicit num: Numeric[V]): TimeSeries[V] = {
    if (ts.isEmpty) ts
    else {
      val startTime = g(ts.index.head)
      // This buffer could be used inside foldLeft, but then Intellij Idea will show wrong errors in += operation.
      val xs = ListBuffer.empty[V]
      ts.dataPoints.foldLeft[(Instant, V)]((startTime, num.zero)) { (acc, x) =>
        val t = g(x._1)
        if (t == acc._1) {
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
  private def mkIndex(start: Instant, end: Instant, delta: Duration): Vector[Instant] = {
    Iterator.iterate(start)(_.plusNanos(delta.toNanos)).takeWhile(_.isBefore(end)).toVector
  }

  /**
    * This function resamples index. But the values are not interpolated but just fractions
    * of current values.
    * For example if we have value 10 every hour and we run step with 15 minutes range
    * we will get values 2.5 every 15 minutes (10/4)
    */
  def step[V: Fractional](ts: TimeSeries[V], d: Duration)(implicit num: Fractional[V]): TimeSeries[V] = {
    val index = mkIndex(ts.index.head, ts.index.last, d)
    val builder: mutable.ListBuffer[V] = ListBuffer()

    @tailrec def stepR(dp: Vector[(Instant, V)], newIdx: Vector[Instant]): Vector[V] = {
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

  def join[V](ts: Seq[TimeSeries[V]]): TimeSeries[Seq[V]] = {
    if (ts.isEmpty) new TimeSeries(Seq())
    else ts.tail.foldLeft(ts.head.mapValues(x => Seq(x))) {
      (acc, x) => acc.join(x).mapValues(y => y._1 :+ y._2)
    }
  }
}

/**
  * TimeSeries contains data indexed by DateTime. The type of stored data
  * is parametric.
  */
case class TimeSeries[V](idx: Vector[Instant], ds: Vector[V]) {

  def this(d: Seq[(Instant, V)]) = {
    this(d.map(_._1).toVector, d.map(_._2).toVector)
  }

  val length: Int = math.min(idx.length, ds.length)
  val index: Vector[Instant] = idx.take(length)
  val values: Vector[V] = ds.take(length)
  val dataPoints: Vector[(Instant, V)] = index.zip(values)

  /** Check is series is empty */
  def isEmpty: Boolean = index.isEmpty || values.isEmpty

  /** Check is series is non empty */
  def nonEmpty: Boolean = !isEmpty

  /** Safe get. If element is out of the bounds then 0 is returned */
  def get(i: Int)(implicit num: Numeric[V]): V = values.lift(i).getOrElse(num.zero)

  def head: Option[(Instant, V)] = {
    for {
      i <- index.headOption
      v <- values.headOption
    } yield (i, v)
  }

  /** Get last element of the series */
  def last: Option[(Instant, V)] = {
    for {
      i <- index.lastOption
      v <- values.lastOption
    } yield (i, v)
  }


  /** Return new Time Series where index is always increasing */
  def sortByIndex: TimeSeries[V] = {
    if (isEmpty) this
    else {
      val ds = dataPoints
      val xs = ListBuffer[(Instant, V)](ds.head)
      ds.tail.foreach { d =>
        if (d._1.isAfter(xs.last._1)) {
          xs.append(d)
        }
      }
      new TimeSeries(xs.toVector)
    }
  }

  /** Filter by index and value */
  def filter(f: ((Instant, V)) => Boolean): TimeSeries[V] = {
    new TimeSeries(index.zip(values).filter(f))
  }

  /** Map by index and value. Create new values */
  def map(f: ((Instant, V)) => V): TimeSeries[V] = {
    val vs: Vector[V] = index.zip(values).map(f)
    new TimeSeries(index, vs)
  }

  /** Map over values. */
  def mapValues[U](f: V => U): TimeSeries[U] = {
    val vs: Vector[U] = values.map(f)
    new TimeSeries(index, vs)
  }

  /** Merges series A into B */
  def merge(ts: TimeSeries[V]): TimeSeries[V] = {
    val res: mutable.ListBuffer[(Instant, V)] = ListBuffer()

    @tailrec
    def insert(xs: Vector[(Instant, V)], ys: Vector[(Instant, V)]): Vector[(Instant, V)] = {

      if (xs.nonEmpty) {
        val x = xs.head
        if (ys.nonEmpty) {
          val y = ys.head
          if (y._1.isBefore(x._1)) {
            res.append(y)
            insert(xs, ys.tail)
          }
          else if (y._1.isAfter(x._1)) {
            res.append((x._1, x._2))
            insert(xs.tail, ys)
          }
          else {
            res.append(x)
            insert(xs.tail, ys.tail)
          }
        }
        else {
          xs.foreach(x => res.append(x))
          res.toVector
        }
      }
      else res.toVector
    }

    val rs = insert(this.dataPoints, ts.dataPoints)
    new TimeSeries[V](rs)
  }


  /** Get slice of series with left side inclusive and right side exclusive
    * this operation is based on index.
    */
  def slice(start: Instant, end: Instant): TimeSeries[V] = {
    val d = index.zip(values).filter(x => (x._1.isAfter(start) || x._1 == start) && x._1.isBefore(end))
    new TimeSeries(d)
  }

  /**
    * Aggregate data points.
    *
    * @param g This function transforms current data point time into new time. All points with the same
    *          time will be integrated into single point
    * @param f This function defines how to aggregate points with the same transformed time
    */
  def groupByTime(g: Instant => Instant, f: Seq[(Instant, V)] => V): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val xs = index.zip(values).toMap
        .groupBy(x => g(x._1))
        .map(x => (x._1, f(x._2.toSeq))).toVector
        .sortBy(_._1)
      new TimeSeries(xs)
    }
  }

  /**
    * Resample given TimeSeries with indexes separated by delta.
    * This function will ensure that series is evenly spaced.
    *
    * @param delta Distance between points
    * @param f     Function which approximates missing points
    */
  def resample(delta: Duration, f: ((Instant, V), (Instant, V), Instant) => V)(implicit num: Numeric[V]): TimeSeries[V] = {
    if (index.isEmpty) TimeSeries.empty[V]
    else {
      val ys: mutable.ListBuffer[V] = ListBuffer()
      val ts = Iterator.iterate(index.head)(_.plusNanos(delta.toNanos))
        .takeWhile(_.isBefore(index.last.plusNanos(1))).toVector

      @tailrec def g(ts: Vector[Instant], xs: Vector[Instant], vs: Vector[V], prev: V): Unit = {
        val tsh = ts.head
        val xsh = xs.head
        if (xsh == tsh) {
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
    def f(x1: (Instant, V), x2: (Instant, V), tsh: Instant) = default

    resample(delta, f)
  }

  /**
    * Add missing points to the time series. The output series will have all its own points
    * and some new points if there are missing at every duration.
    *
    * @param delta Expected distance between points
    * @param f     This function will approximate missing points.
    */
  def addMissing(delta: Duration, f: ((Instant, V), (Instant, V), Instant) => V): TimeSeries[V] = {
    if (isEmpty) this
    else {
      val ys: mutable.ListBuffer[(Instant, V)] = ListBuffer()
      val resampledIndex = mkIndex(index.head, index.last, delta)

      @tailrec def g(ts: Vector[Instant], xs: Vector[(Instant, V)], prev: (Instant, V)): Unit = {
        if (xs.nonEmpty) {
          val xsh = xs.head
          if (ts.isEmpty) {
            ys.append(xs.head)
            g(ts, xs.tail, xsh)

          } else if (xsh._1 == ts.head) {
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

    @tailrec def g(v: Vector[(Instant, V)], out: Vector[V]): Vector[V] = {
      if (v.nonEmpty) {
        val splitIndex: Int = v.indexWhere(x => x._1.isBefore(v.head._1.minus(windowSize).minusNanos(1)))
        val window = if (splitIndex > 0) v.take(splitIndex) else v
        g(v.tail, out :+ f(window.map(x => x._2)))
      }
      else out
    }

    new TimeSeries(index, g(index.zip(values).reverse, Vector.empty).reverse)
  }

  /** Shift index by specific time duration */
  def shiftTime(d: Duration): TimeSeries[V] = {
    TimeSeries(index.map(_.plus(d)), values)
  }

  /** Inner join. Only include points which have the same data in both series */
  def join[U](ts: TimeSeries[U]): TimeSeries[(V, U)] = {
    val builder: mutable.ListBuffer[(Instant, (V, U))] = ListBuffer()

    @tailrec def joinR(xs: Vector[(Instant, V)],
                       ys: Vector[(Instant, U)]): Seq[(Instant, (V, U))] = {
      if (xs.isEmpty || ys.isEmpty) builder
      else {
        val x = xs.head
        val y = ys.head
        if (x._1 == y._1) {
          builder.append((x._1, (x._2, y._2)))
          joinR(xs.tail, ys.tail)
        }
        else if (x._1.isBefore(y._1)) joinR(xs.tail, ys)
        else joinR(xs, ys.tail)

      }
    }

    new TimeSeries(joinR(dataPoints, ts.dataPoints))
  }

  /** Left join. If right series doesn't have a data point then put default value. */
  def joinLeft[U](ts: TimeSeries[U], default: U): TimeSeries[(V, U)] = {
    val builder: mutable.ListBuffer[(Instant, (V, U))] = ListBuffer()

    @tailrec def joinR(xs: Vector[(Instant, V)],
                       ys: Vector[(Instant, U)]): Seq[(Instant, (V, U))] = {
      if (xs.isEmpty || ys.isEmpty) builder
      else {
        val x = xs.head
        val y = ys.head
        if (x._1 == y._1) {
          builder.append((x._1, (x._2, y._2)))
          joinR(xs.tail, ys.tail)
        } else if (x._1.isBefore(y._1)) {
          builder.append((x._1, (x._2, default)))
          joinR(xs.tail, ys)
        } else joinR(xs, ys.tail)
      }
    }

    new TimeSeries(joinR(dataPoints, ts.dataPoints))
  }

  /** Outer join. If one of the series doesn't have a data point then put default(left or right) value. */
  def joinOuter[U](ts: TimeSeries[U], defaultLeft: V, defaultRight: U): TimeSeries[(V, U)] = {
    val builder: mutable.ListBuffer[(Instant, (V, U))] = ListBuffer()

    @tailrec def joinR(xs: Vector[(Instant, V)],
                       ys: Vector[(Instant, U)]): Seq[(Instant, (V, U))] = {
      if (xs.isEmpty) {
        ys.foreach(y => builder.append((y._1, (defaultLeft, y._2))))
        builder
      }
      else if (ys.isEmpty) {
        xs.foreach(x => builder.append((x._1, (x._2, defaultRight))))
        builder
      }
      else {
        val x = xs.head
        val y = ys.head
        if (x._1 == y._1) {
          builder.append((x._1, (x._2, y._2)))
          joinR(xs.tail, ys.tail)
        } else if (x._1.isBefore(y._1)) {
          builder.append((x._1, (x._2, defaultRight)))
          joinR(xs.tail, ys)
        } else {
          builder.append((y._1, (defaultLeft, y._2)))
          joinR(xs, ys.tail)
        }
      }
    }

    new TimeSeries(joinR(dataPoints, ts.dataPoints))
  }
}

