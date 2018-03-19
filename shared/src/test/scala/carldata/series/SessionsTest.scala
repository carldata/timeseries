package carldata.series

import java.time.Duration

import carldata.series.Sessions.Session
import org.scalatest._

class SessionsTest extends FlatSpec with Matchers {
  private val seriesOfTypicalRain = Csv.fromString(
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

  private val seriesOfEvanescentRain = Csv.fromString(
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

  private val seriesOfAllRain = Csv.fromString(
    """time,value
      |2008-01-01T12:35:00, 4
      |2008-01-01T12:36:00, 5
      |2008-01-01T12:37:00, 7
      |2008-01-01T12:38:00, 1
      |2008-01-01T12:39:00, 4
      |2008-01-01T12:40:00, 5
      |2008-01-01T12:41:00, 6
      |2008-01-01T12:42:00, 4
      |2008-01-01T12:43:00, 2
      |2008-01-01T12:44:00, 5""".stripMargin)

  private val seriesOfNoRain = Csv.fromString(
    """time,value
      |2008-01-01T12:35:00, 0
      |2008-01-01T12:36:00, 0
      |2008-01-01T12:37:00, 0
      |2008-01-01T12:38:00, 0
      |2008-01-01T12:39:00, 0
      |2008-01-01T12:40:00, 0
      |2008-01-01T12:41:00, 0
      |2008-01-01T12:42:00, 0
      |2008-01-01T12:43:00, 0
      |2008-01-01T12:44:00, 0""".stripMargin)

  "findSessions" should "detect sessions" in {
    val expected: Seq[Session] = Seq(Session(0,2), Session(7,8), Session(10,17), Session(23,24), Session(28,29))
    Sessions.findSessions(seriesOfTypicalRain) shouldBe expected
  }

  it should "detect one session in signal of all non-zero values" in {
    val expected: Seq[Session] = Seq(Session(0, 9))
    Sessions.findSessions(seriesOfAllRain) shouldBe expected
  }

  it should "detect no session in signal of all zero values" in {
    val expected: Seq[Session] = Seq()
    Sessions.findSessions(seriesOfNoRain) shouldBe expected
  }

  "findSessionsWithTolerance" should "detect sessions with toleration of 1 minute in typical rain signal" in {
    val expected = Seq(Session(0,2), Session(7,8), Session(10,17), Session(23,24), Session(28,29))
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "detect sessions with toleration of 3 minutes in typical rain signal" in {
    val expected = Seq(Session(0,2), Session(7,17), Session(23,24), Session(28,29))
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(3)) shouldBe expected
  }

  it should "detect sessions with toleration of 4 minutes in typical rain signal" in {
    val expected = Seq(Session(0,2), Session(7,17), Session(23,29))
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(4)) shouldBe expected
  }

  it should "detect sessions with toleration of 0 minutes in evanescent rain signal" in {
    val expected = Seq(Session(2,2), Session(4,4), Session(7,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(0)) shouldBe expected
  }

  it should "detect sessions with toleration of 1 minute in evanescent rain signal" in {
    val expected = Seq(Session(2,2), Session(4,4), Session(7,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "detect sessions with toleration of 2 minutes in evanescent rain signal" in {
    val expected = Seq(Session(2,4), Session(7,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(2)) shouldBe expected
  }

  it should "detect sessions with toleration of 3 minutes in evanescent rain signal" in {
    val expected = Seq(Session(2,7), Session(11,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(3)) shouldBe expected
  }

  it should "detect sessions with toleration of 4 minutes in evanescent rain signal" in {
    val expected = Seq(Session(2,11), Session(16,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(4)) shouldBe expected
  }

  it should "detect sessions with toleration of 5 minutes in evanescent rain signal" in {
    val expected = Seq(Session(2,16), Session(22,22))
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(5)) shouldBe expected
  }

  it should "return proper result for toleration greater than signal" in {
    val expected = Seq(Session(0, 29))
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(100)) shouldBe expected
  }
}