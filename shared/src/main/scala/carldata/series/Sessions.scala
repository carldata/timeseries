package carldata.series

import java.time.{Duration, Instant}

object Sessions {

  case class Session(startIndex: Instant, endIndex: Instant)

  /**
    * Session is continuous period where the data point value is not 0
    */
  def findSessions[V: Numeric](ts: TimeSeries[V]): Seq[Session] = {
    val xs: Vector[((Instant, V), (Instant, V))] = ts.dataPoints.zip(ts.dataPoints.tail)
    val xsHead: List[Session] = if (xs.nonEmpty && xs.head._2._2 != 0)
      List(Session(xs.head._1._1, xs.head._1._1))
    else
      List()

    xs.foldLeft[List[Session]](xsHead) { (z, e) =>

      if ((e._1._2 == 0) && (e._2._2 != 0))
        Session(e._2._1, e._2._1) :: z
      else if (e._1._2 != 0)
        Session(z.head.startIndex, if (e._2._2 != 0) e._2._1 else e._1._1) :: z.tail
      else
        z
    }.reverse
  }

  /** Finds sessions with a duration tolerance
    *
    * @param tolerance the time distance (inclusive) between two detected sessions that will make the sessions merged,
    *                  for example, for a fragment of signal values distributed evenly for exactly one second
    *                  (2, 0, 1, 0, 0, 2, 3, 4, 5, 7, 0)
    *                  the Duration.ofSeconds(1) will be following sessions detected: [Session(0,0), Session(2,2), Session(5, 9)]
    *                  the Duration.ofSeconds(2) will be following sessions detected: [Session(0,2), Session(5, 9)]
    */
  def findSessions[V: Numeric](ts: TimeSeries[V], tolerance: Duration)(implicit num: Fractional[V]): Seq[Session] = {
    val xs = Sessions.findSessions(ts)
    if (xs.isEmpty) Seq()
    else
      xs.tail.foldLeft[List[Session]](List(xs.head))((zs, x) => {
        if (tolerance.compareTo(Duration.between(zs.head.endIndex, x.startIndex)) >= 0)
          Session(zs.head.startIndex, x.endIndex) :: zs.tail //merge sessions
        else
          x :: zs
      }).reverse
  }

  /**
    * Time series adjusted to session, inclusive
    */
  def splitBySession[V: Numeric](ts: TimeSeries[V], session: Session): TimeSeries[V] = {
    ts.slice(session.startIndex, session.endIndex.plusSeconds(1))
  }
}