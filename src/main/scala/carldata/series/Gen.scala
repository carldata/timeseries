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
      if(pos.isBefore(end) || pos == end){
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
}
