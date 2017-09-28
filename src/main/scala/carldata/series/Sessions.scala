package carldata.series

import java.time.{Duration, LocalDateTime}

import scala.collection.immutable

object Sessions {

  case class Session(startIndex: Int, endIndex: Int)

  def findSessions[V: Numeric](ts: TimeSeries[V]): Seq[Session] = {
    val tss = ts.values.zipWithIndex
    // where: (_1, _2, _3) = (index of the first of compared pair, first pair value, second pair value)
    val zps: Seq[(Int, V, V)] = tss.zip(tss.tail).map(e => (e._1._2, e._1._1, e._2._1))
    val zStart: List[Session] = if (zps.nonEmpty && zps.head._2 != 0)
      List(Session(zps.head._1, zps.head._1))
    else
      List()
    zps.foldLeft[List[Session]](zStart) { (z, e) =>
      if ((e._2 == 0) && (e._3 != 0))
        Session(e._1 + 1, e._1 + 1) :: z
      else if (e._2 != 0)
        Session(z.head.startIndex, e._1 + (if (e._3 != 0) 1 else 0)) :: z.tail
      else
        z
    }.reverse
  }

  /** Finds sessions with a duration tolerance
    * @param tolerance the time distance (inclusive) between two detected sessions that will make the sessions merged,
    *                  for example, for a fragment of signal values distributed evenly for exactly one second
    *                  (2, 0, 1, 0, 0, 2, 3, 4, 5, 7, 0)
    *                  the Duration.ofSeconds(1) will be following sessions detected: [Session(0,0), Session(2,2), Session(5, 9)]
    *                  the Duration.ofSeconds(2) will be following sessions detected: [Session(0,2), Session(5, 9)]
    * */

  def findSessions[V: Numeric](ts: TimeSeries[V], tolerance: Duration)(implicit num: Fractional[V]): Seq[Session] = {
    if (tolerance.compareTo(Duration.between(ts.head.get._1, ts.last.get._1)) >= 0)
      Seq(Session(0, ts.length - 1))
    else {
      val xs = Sessions.findSessions(ts)
      val is = ts.index.zipWithIndex.map(x => (x._2, x._1))
      xs.tail.foldLeft[Seq[Session]](Seq(xs.head))((zs, x) => {
        if (tolerance.compareTo(Duration.between(is(zs.head.endIndex)._2, is(x.startIndex)._2)) >= 0)
          Session(zs.head.startIndex, x.endIndex) +: zs.tail //merge sessions
        else
          x +: zs
      }).reverse
    }
  }
}