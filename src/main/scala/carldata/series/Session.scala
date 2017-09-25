package carldata.series

case class Session(startIndex: Int, endIndex: Int) {
}

object Session {
  def findSessions[V: Fractional](ts: TimeSeries[V]): Seq[Session] = {
    val tss = ts.values.zipWithIndex //time series values indexed
    val zps: Seq[((V, Int), (V, Int))] = tss.zip(tss.tail).dropWhile(x => x._1._1 == 0) //zipped pairs
    val zStart: Seq[Session] = if (zps.nonEmpty && zps.head._1._1 != 0)
      Seq(Session(zps.head._1._2, zps.head._1._2))
    else
      Seq()
    zps.foldLeft[Seq[Session]](zStart) {
      (z: Seq[Session], e: ((V, Int), (V, Int))) => {
        // where:
        //  e._1._1 - value of the first of compared pair (x, y)
        //  e._2._1 - value of the second of compared pair (x, y)
        //  e._1._2 - index of the first of compared pair (x, y)
        //  e._2._2 - index of the second of compared pair (x, y)
        if ((e._1._1 == 0) && (e._2._1  != 0))
          Session(e._2._2, e._2._2) +: z
        else if (e._1._1 != 0)
          Session(z.head.startIndex, e._2._2 + (if (e._2._1 != 0) 0 else -1)) +: z.tail
        else
          z
      }
    }.reverse
  }
}