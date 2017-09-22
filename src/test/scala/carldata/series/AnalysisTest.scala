package carldata.series

import org.scalatest._
import carldata.series.Analysis._

class AnalysisTest extends FlatSpec with Matchers {

  "findSessions" should "detect sessions" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 4), (2, 6), (3, 5), (4, 3), (5, 0), (6, 4), (7, 6), (8, 3), (9, 1), (10, 0), (11, 7), (12, 8)))
    val sessions = series.findSessions
    sessions.length shouldBe 3
    val firstDetected = sessions(0)
    firstDetected.startIndex shouldBe 0
    firstDetected.endIndex shouldBe 3
    val secondDetected = sessions(1)
    secondDetected.startIndex shouldBe 5
    secondDetected.endIndex shouldBe 8
    val thirdDetected = sessions(2)
    thirdDetected.startIndex shouldBe 10
    thirdDetected.endIndex shouldBe 11
  }

  "findSessions" should "detect sessions distributed in consecutive zeros signal" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 0), (2, 0), (3, 0), (4, 1), (5, 1), (6, 0), (7, 0)))
    val sessions = series.findSessions
    sessions.length shouldBe 1
    val firstDetected = sessions(0)
    firstDetected.startIndex shouldBe 3
    firstDetected.endIndex shouldBe 4
  }

  "findSessions" should "detect sessions placed in the beginning and in the end of signal" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 1), (3, 0), (4, 1), (5, 0), (6, 1), (7, 1)))
    val sessions = series.findSessions
    sessions.length shouldBe 3
    val firstDetected = sessions(0)
    firstDetected.startIndex shouldBe 0
    firstDetected.endIndex shouldBe 1
    val thirdDetected = sessions(2)
    thirdDetected.startIndex shouldBe 5
    thirdDetected.endIndex shouldBe 6
  }
}