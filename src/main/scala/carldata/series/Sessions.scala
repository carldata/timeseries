package carldata.series

import java.time.Duration

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
        Session(e._1+1, e._1+1) :: z
      else if (e._2 != 0)
        Session(z.head.startIndex, e._1 + (if (e._3 != 0) 1 else 0)) :: z.tail
      else
        z
    }.reverse
  }


  def findSessions[V: Numeric](ts: TimeSeries[V], maxTolerance: Duration)(implicit num: Fractional[V]): Seq[Session] = {
    if (maxTolerance.compareTo(Duration.between(ts.head.get._1, ts.last.get._1)) >= 0)
      Seq(Session(0, ts.length-1))
    else {
      val seriesRolled = ts.rollingWindow(maxTolerance, sx => sx.sum)
      Sessions.findSessions[V](seriesRolled.map(x =>
        if (seriesRolled.slice(x._1, x._1.plusMinutes(maxTolerance.toMinutes).plusNanos(1)).values.contains(0))
          num.fromInt(0)
        else
          x._2
      ))
    }
  }
}