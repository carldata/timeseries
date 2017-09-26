package carldata.series

import java.time.Duration

import carldata.series.Sessions.Session
import org.scalatest._

class SessionsTest extends FlatSpec with Matchers {
  val seriesStartsWetEndsWet = Csv.fromString(
    """time,value
      |2008-01-01T12:35:00, 4
      |2008-01-01T12:36:00, 2
      |2008-01-01T12:37:00, 3
      |2008-01-01T12:38:00, 0
      |2008-01-01T12:39:00, 0
      |2008-01-01T12:40:00, 0
      |2008-01-01T12:41:00, 0
      |2008-01-01T12:42:00, 1
      |2008-01-01T12:43:00, 2
      |2008-01-01T12:44:00, 0
      |2008-01-01T12:45:00, 1
      |2008-01-01T12:46:00, 2
      |2008-01-01T12:47:00, 3
      |2008-01-01T12:48:00, 2
      |2008-01-01T12:49:00, 5
      |2008-01-01T12:50:00, 6
      |2008-01-01T12:51:00, 7
      |2008-01-01T12:52:00, 1
      |2008-01-01T12:53:00, 0
      |2008-01-01T12:54:00, 0
      |2008-01-01T12:55:00, 0
      |2008-01-01T12:56:00, 0
      |2008-01-01T12:57:00, 0
      |2008-01-01T12:58:00, 2
      |2008-01-01T12:59:00, 4
      |2008-01-01T13:00:00, 0
      |2008-01-01T13:01:00, 0
      |2008-01-01T13:02:00, 0
      |2008-01-01T13:03:00, 1
      |2008-01-01T13:04:00, 3""".stripMargin)

  val seriesWithGaps = Csv.fromString(
    """time,value
      |2008-01-01T12:35:00, 4
      |2008-01-01T12:36:00, 2
      |2008-01-01T12:37:00, 3
      |2008-01-01T12:38:00, 0
      |2008-01-01T12:39:00, 0
      |2008-01-01T12:40:00, 0
      |2008-01-01T12:41:00, 0
      |2008-01-01T12:42:00, 1
      |2008-01-01T12:44:00, 0
      |2008-01-01T12:45:00, 1
      |2008-01-01T12:46:00, 2
      |2008-01-01T12:48:00, 2
      |2008-01-01T12:49:00, 5
      |2008-01-01T12:50:00, 6
      |2008-01-01T12:51:00, 7
      |2008-01-01T12:52:00, 1
      |2008-01-01T12:53:00, 0
      |2008-01-01T12:54:00, 0
      |2008-01-01T12:56:00, 0
      |2008-01-01T12:57:00, 0
      |2008-01-01T12:58:00, 2
      |2008-01-01T12:59:00, 4
      |2008-01-01T13:01:00, 0
      |2008-01-01T13:02:00, 0
      |2008-01-01T13:03:00, 1
      |2008-01-01T13:04:00, 3""".stripMargin)

  val seriesOfEvanescentRain = Csv.fromString(
    """time,value
      |2008-01-01T12:35:00, 0
      |2008-01-01T12:36:00, 0
      |2008-01-01T12:37:00, 1
      |2008-01-01T12:38:00, 0
      |2008-01-01T12:39:00, 1
      |2008-01-01T12:40:00, 0
      |2008-01-01T12:41:00, 0
      |2008-01-01T12:42:00, 1
      |2008-01-01T12:43:00, 0
      |2008-01-01T12:44:00, 0
      |2008-01-01T12:45:00, 0
      |2008-01-01T12:46:00, 1
      |2008-01-01T12:47:00, 0
      |2008-01-01T12:48:00, 0
      |2008-01-01T12:49:00, 0
      |2008-01-01T12:50:00, 0
      |2008-01-01T12:51:00, 1
      |2008-01-01T12:52:00, 0
      |2008-01-01T12:53:00, 0
      |2008-01-01T12:54:00, 0
      |2008-01-01T12:55:00, 0
      |2008-01-01T12:56:00, 0
      |2008-01-01T12:57:00, 1
      |2008-01-01T12:58:00, 0
      |2008-01-01T12:59:00, 0
      |2008-01-01T13:00:00, 0
      |2008-01-01T13:01:00, 0
      |2008-01-01T13:02:00, 0
      |2008-01-01T13:03:00, 0
      |2008-01-01T13:04:00, 0""".stripMargin)

  "findSessions" should "detect sessions" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 4), (2, 6), (3, 5), (4, 3), (5, 0), (6, 4), (7, 6), (8, 3), (9, 1), (10, 0), (11, 7), (12, 8)))
    val expected: Seq[Session] = Seq(Session(0, 3), Session(5, 8), Session(10, 11))
    Sessions.findSessions(series) shouldBe expected
  }

  "findSessions" should "detect sessions distributed in consecutive zeros signal" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 0), (2, 0), (3, 0), (4, 1), (5, 1), (6, 0), (7, 0)))
    val expected: Seq[Session] = Seq(Session(3, 4))
    Sessions.findSessions(series) shouldBe expected
  }

  "findSessions" should "detect sessions placed in the beginning and in the end of signal" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 1), (3, 0), (4, 1), (5, 0), (6, 1), (7, 1)))
    val expected: Seq[Session] = Seq(Session(0, 1), Session(3, 3), Session(5, 6))
    Sessions.findSessions(series) shouldBe expected
  }

  "findSessions" should "detect one session in signal of all non-zero values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 1), (2, 1), (3, 5), (4, 1), (5, 5), (6, 1), (7, 1)))
    val expected: Seq[Session] = Seq(Session(0, 6))
    Sessions.findSessions(series) shouldBe expected
  }

  "findSessions" should "detect no session in signal of all zero values" in {
    val series: TimeSeries[Double] = TimeSeries.fromTimestamps(Seq((1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)))
    val expected: Seq[Session] = Seq()
    Sessions.findSessions(series) shouldBe expected
  }

  "findSessionsWithTolerance" should "detect session" in {
    Sessions.findSessions(seriesStartsWetEndsWet, Duration.ofMinutes(1)) shouldBe Seq(Session(0, 2), Session(7, 17), Session(23, 24), Session(28, 29))
    Sessions.findSessions(seriesStartsWetEndsWet, Duration.ofMinutes(3)) shouldBe Seq(Session(0, 2), Session(7, 17), Session(23, 29))
    Sessions.findSessions(seriesStartsWetEndsWet, Duration.ofMinutes(4)) shouldBe Seq(Session(0, 17), Session(23, 29))

    Sessions.findSessions(seriesWithGaps, Duration.ofMinutes(1)) shouldBe Seq(Session(0, 2), Session(7, 7), Session(9, 15), Session(20, 21), Session(24, 25))
    Sessions.findSessions(seriesWithGaps, Duration.ofMinutes(3)) shouldBe Seq(Session(0, 2), Session(7, 15), Session(20, 25))
    Sessions.findSessions(seriesWithGaps, Duration.ofMinutes(4)) shouldBe Seq(Session(0, 15), Session(20, 25))

    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(0)) shouldBe Seq(Session(2,2), Session(4,4), Session(7,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(1)) shouldBe Seq(Session(2,4), Session(7,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(2)) shouldBe Seq(Session(2,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(3)) shouldBe Seq(Session(2,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(4)) shouldBe Seq(Session(2,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(5)) shouldBe Seq(Session(2, 22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(100)) shouldBe Seq(Session(0, 29))
  }
}