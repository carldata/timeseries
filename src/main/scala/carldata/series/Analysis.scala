package carldata.series

import java.time.LocalDateTime


object Analysis {
  implicit class SeriesAnalysis[V](val ts: TimeSeries[V])(implicit num: Numeric[V]) {

    def findSessions: Seq[Session] = {
      /**
        * Auxiliary class for storing value of a TimeSeries and its associated index in TimeSeries (where it can be found)
        */
      case class IndexValue(index: Int, value: V)

      /**
        * First get rid of the time dimension in _x values of tuples hold in TimeSeries
        * and introduce the index of element instead
        */
      val timeSeriesValuesIndexed = ts.dataPoints.foldLeft[Seq[IndexValue]](Seq()) {
        (x: Seq[IndexValue], y: (LocalDateTime, V)) => x :+ IndexValue(x.length, y._2)
      }

      /**
        * Create a sequence of samples, where every sample is a tuple of (preceding, succeeding)
        */
      val zippedPairs: Seq[(IndexValue, IndexValue)] = timeSeriesValuesIndexed.zip(timeSeriesValuesIndexed.tail)

      zippedPairs.foldLeft[Seq[Session]](Seq()) {
        (z: Seq[Session], e: (IndexValue, IndexValue)) => {
          var result = z
          val preceding = e._1
          val succeeding = e._2

          // definitely a corner case - but most often we start with this one:
          // preceding value of the pair is non-zero and we
          // did not detected any sessions so far - add a new Session element right now !
          if ((preceding.value != 0) && (z.length == 0))
            result = Seq(Session(preceding.index, succeeding.index))

          // typical case - detected the start of session
          if ((preceding.value == 0) && (succeeding.value != 0))
            result = Session(succeeding.index, succeeding.index) +: z

          // typical case - detected the end of session, and the session start index
          // is passed in the head.startIndex value (since session start was previously detected)
          if ((preceding.value != 0) && (succeeding.value == 0) && (z.length > 0))
            result = Session(z.head.startIndex, succeeding.index-1) +: z.tail

          // kind of a corner case - always update the end index, since e: (IndexValue, IndexValue)
          // might be the last pair analysed !
          if ((preceding.value != 0) && (succeeding.value != 0) && (z.length > 0))
            result = Session(z.head.startIndex, succeeding.index) +: z.tail

          result
        }
      }.reverse
    }
  }
}
