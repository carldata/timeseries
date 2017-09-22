package carldata.series

import org.scalatest._
import carldata.series.Analysis._

class AnalysisTest extends FlatSpec with Matchers {

  "findSessions" should "detect sessions" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 4), (2, 6), (3, 5), (4, 3), (5, 0), (6, 4), (7, 6), (8, 3), (9, 1), (10, 0), (11, 7), (12, 8)))
    val expected: Seq[Session] = Seq(Session(0, 3), Session(5, 8), Session(10, 11))
    series.findSessions shouldBe expected
  }

  "findSessions" should "detect sessions distributed in consecutive zeros signal" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 0), (2, 0), (3, 0), (4, 1), (5, 1), (6, 0), (7, 0)))
    val expected: Seq[Session] = Seq(Session(3, 4))
    series.findSessions shouldBe expected
  }

  "findSessions" should "detect sessions placed in the beginning and in the end of signal" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 1), (3, 0), (4, 1), (5, 0), (6, 1), (7, 1)))
    val expected: Seq[Session] = Seq(Session(0, 1), Session(3, 3), Session(5, 6))
    series.findSessions shouldBe expected
  }

  "findSessions" should "detect one session in signal of all non-zero values" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 1), (3, 5), (4, 1), (5, 5), (6, 1), (7, 1)))
    val expected: Seq[Session] = Seq(Session(0, 6))
    series.findSessions shouldBe expected
  }

  "findSessions" should "detect no session in signal of all zero values" in {
    val series: TimeSeries[Int] = TimeSeries.fromTimestamps(Seq((1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)))
    val expected: Seq[Session] = Seq()
    series.findSessions shouldBe expected
  }
}