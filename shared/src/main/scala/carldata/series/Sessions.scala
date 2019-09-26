package carldata.series

import java.time.{Duration, Instant}

import scala.annotation.tailrec

object Sessions {

  case class Session(startIndex: Instant, endIndex: Instant)

  /**
    * Session is continuous period where the data point value is not 0
    */
  def findSessions[V: Numeric](ts: TimeSeries[V])(implicit num: Numeric[V]): Seq[Session] = {
    @tailrec
    def f(xs: Stream[(Instant, V)], currentSession: Option[Session], sessions: Seq[Session]): Seq[Session] = {
      if (xs.isEmpty) currentSession.map(cs => sessions :+ cs).getOrElse(sessions)
      else {
        val head = xs.head
        if (head._2 != num.zero) {
          val widerSession: Session = currentSession.map(cs => Session(cs.startIndex, head._1))
            .getOrElse(Session(head._1, head._1))
          f(xs.tail, Some(widerSession), sessions)
        }
        else {
          f(xs.tail, None, currentSession.map(cs => sessions :+ cs).getOrElse(sessions))
        }
      }
    }

    f(ts.dataPoints.toStream, None, Seq())
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