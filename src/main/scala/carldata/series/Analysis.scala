package carldata.series

object Analysis {
  implicit class SeriesAnalysis[V](val ts: TimeSeries[V])(implicit num: Numeric[V]) {

    def findSessions: Seq[Session] = {
      val timeSeriesValuesIndexed = ts.values.zipWithIndex
      val zippedPairs: Seq[((V, Int), (V, Int))] = timeSeriesValuesIndexed.zip(timeSeriesValuesIndexed.tail).dropWhile(x => x._1._1 == 0)
      val zStart: Seq[Session] = if (zippedPairs.nonEmpty && zippedPairs.head._1._1 != 0)
        Seq(Session(zippedPairs.head._1._2, zippedPairs.head._1._2))
      else
        Seq()
      zippedPairs.foldLeft[Seq[Session]](zStart) {
        (z: Seq[Session], e: ((V, Int), (V, Int))) => {
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
}
