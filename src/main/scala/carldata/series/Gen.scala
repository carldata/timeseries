package carldata.series

import java.time.{Duration, Instant}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Time Series generators
  */
object Gen {

  /** Helper function for creating index entries */
  def mkIndex(start: Instant, end: Instant, duration: Duration): Vector[Instant] = {
    val xs = ListBuffer[Instant]()

    @tailrec def appendR(pos: Instant): Unit = {
      if (pos.isBefore(end) || pos == end) {
        xs += pos
        appendR(pos.plus(duration))
      }
    }

    appendR(start)
    xs.toVector
  }

  /** Generate time series with constant value */
  def constant[V](idx: Vector[Instant], v: V): TimeSeries[V] = {
    TimeSeries(idx, idx.map(_ => v))
  }

  /** Generate time series by repeat pattern between start and end date */
  def repeat[V](pattern: TimeSeries[V], start: Instant, end: Instant): TimeSeries[V] = {
    @tailrec def repeatR(dp: (Vector[Instant], Vector[V]), res: (Vector[Instant], Vector[V])): (Vector[Instant], Vector[V]) = {
      if (res._1.last.isBefore(end)) {
        val head = (dp._1.head, dp._2.head)
        val tail = (dp._1.tail, dp._2.tail)
        if (tail._1.isEmpty) {
          val dt = pattern.index(1).minusSeconds(pattern.index.head.getEpochSecond)
          repeatR((pattern.index, pattern.values), (res._1.:+(res._1.last.plusSeconds(dt.getEpochSecond)), res._2.:+(pattern.values.head)))
        }
        else {
          val dt = tail._1.head.minusSeconds(head._1.getEpochSecond)
          repeatR(tail, (res._1.:+(res._1.last.plusSeconds(dt.getEpochSecond)), res._2.:+(tail._2.head)))
        }
      }
      else res
    }

    if (pattern.isEmpty) pattern
    else {

      val (idx, vs) = repeatR((pattern.index, pattern.values), (Vector(start), Vector(pattern.values.head)))
      TimeSeries(idx, vs)
    }
  }

}
