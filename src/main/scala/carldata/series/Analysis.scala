package carldata.series

import scala.annotation.tailrec

object Analysis {
  implicit class SeriesAnalysis[V](val ts: TimeSeries[V])(implicit num: Numeric[V]) {

    def findSessions: Seq[Session] = {
      @tailrec def traverseTimeSeries[V](ts: TimeSeries[V],
                                         sessionStartIndex: Int,
                                         headIndex: Int,
                                         detectedSession: Boolean,
                                         result: Seq[Session]): Seq[Session] = {
        ts match {
          case TimeSeries(_ +: idxTail, dsHead +: dsTail) => {
            if (dsHead != 0) {
              traverseTimeSeries(TimeSeries(idxTail, dsTail),
                sessionStartIndex,
                headIndex + 1,
                true,
                result)
            }
            else {
              traverseTimeSeries(TimeSeries(idxTail, dsTail),
                headIndex + 1,
                headIndex + 1,
                false,
                if (detectedSession) result :+ Session(sessionStartIndex, headIndex - 1) else result)
            }
          }
          case _ => if (detectedSession) result :+ Session(sessionStartIndex, headIndex - 1) else result
        }
      }

      traverseTimeSeries[V](ts, 0, 0, false, Seq())
    }
  }
}
