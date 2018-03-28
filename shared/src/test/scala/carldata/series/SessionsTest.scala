package carldata.series

import java.time.{Duration, Instant}

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
      |2008-01-01T13:04:00, 3""".stripMargin).head

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
      |2008-01-01T13:04:00, 0""".stripMargin).head

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
      |2008-01-01T12:44:00, 5""".stripMargin).head

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
      |2008-01-01T12:44:00, 0""".stripMargin).head

  def createSession(x: (String, String)): Session = Session(Instant.parse(x._1), Instant.parse(x._2))

  "findSessions" should "detect sessions" in {
    val expected: Seq[Session] = Seq(("2008-01-01T12:35:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:42:00Z", "2008-01-01T12:43:00Z")
      , ("2008-01-01T12:45:00Z", "2008-01-01T12:52:00Z"), ("2008-01-01T12:58:00Z", "2008-01-01T12:59:00Z")
      , ("2008-01-01T13:03:00Z", "2008-01-01T13:04:00Z")).map(createSession)
    Sessions.findSessions(seriesOfTypicalRain) shouldBe expected
  }

  it should "detect one session in signal of all non-zero values" in {
    val expected: Seq[Session] = Seq(("2008-01-01T12:35:00Z", "2008-01-01T12:44:00Z")).map(createSession)
    Sessions.findSessions(seriesOfAllRain) shouldBe expected
  }

  it should "detect no session in signal of all zero values" in {
    val expected: Seq[Session] = Seq()
    Sessions.findSessions(seriesOfNoRain) shouldBe expected
  }

  "findSessionsWithTolerance" should "detect sessions with toleration of 1 minute in typical rain signal" in {
    val expected = Seq(("2008-01-01T12:35:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:42:00Z", "2008-01-01T12:43:00Z")
      , ("2008-01-01T12:45:00Z", "2008-01-01T12:52:00Z"), ("2008-01-01T12:58:00Z", "2008-01-01T12:59:00Z")
      , ("2008-01-01T13:03:00Z", "2008-01-01T13:04:00Z")).map(createSession)
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "detect sessions with toleration of 3 minutes in typical rain signal" in {
    val expected = Seq(("2008-01-01T12:35:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:42:00Z", "2008-01-01T12:52:00Z")
      , ("2008-01-01T12:58:00Z", "2008-01-01T12:59:00Z"), ("2008-01-01T13:03:00Z", "2008-01-01T13:04:00Z")).map(createSession)
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(3)) shouldBe expected
  }

  it should "detect sessions with toleration of 4 minutes in typical rain signal" in {
    val expected = Seq(("2008-01-01T12:35:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:42:00Z", "2008-01-01T12:52:00Z")
      , ("2008-01-01T12:58:00Z", "2008-01-01T13:04:00Z")).map(createSession)
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(4)) shouldBe expected
  }

  it should "detect sessions with toleration of 0 minutes in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:39:00Z", "2008-01-01T12:39:00Z")
      , ("2008-01-01T12:42:00Z", "2008-01-01T12:42:00Z"), ("2008-01-01T12:46:00Z", "2008-01-01T12:46:00Z")
      , ("2008-01-01T12:51:00Z", "2008-01-01T12:51:00Z"), ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(0)) shouldBe expected
  }

  it should "detect sessions with toleration of 1 minute in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:37:00Z"), ("2008-01-01T12:39:00Z", "2008-01-01T12:39:00Z")
      , ("2008-01-01T12:42:00Z", "2008-01-01T12:42:00Z"), ("2008-01-01T12:46:00Z", "2008-01-01T12:46:00Z")
      , ("2008-01-01T12:51:00Z", "2008-01-01T12:51:00Z"), ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(1)) shouldBe expected
  }

  it should "detect sessions with toleration of 2 minutes in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:39:00Z"), ("2008-01-01T12:42:00Z", "2008-01-01T12:42:00Z")
      , ("2008-01-01T12:46:00Z", "2008-01-01T12:46:00Z"), ("2008-01-01T12:51:00Z", "2008-01-01T12:51:00Z")
      , ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(2)) shouldBe expected
  }

  it should "detect sessions with toleration of 3 minutes in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:42:00Z"), ("2008-01-01T12:46:00Z", "2008-01-01T12:46:00Z")
      , ("2008-01-01T12:51:00Z", "2008-01-01T12:51:00Z"), ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(3)) shouldBe expected
  }

  it should "detect sessions with toleration of 4 minutes in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:46:00Z"), ("2008-01-01T12:51:00Z", "2008-01-01T12:51:00Z")
      , ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(4)) shouldBe expected
  }

  it should "detect sessions with toleration of 5 minutes in evanescent rain signal" in {
    val expected = Seq(("2008-01-01T12:37:00Z", "2008-01-01T12:51:00Z")
      , ("2008-01-01T12:57:00Z", "2008-01-01T12:57:00Z")).map(createSession)
    Sessions.findSessions(seriesOfEvanescentRain, Duration.ofMinutes(5)) shouldBe expected
  }

  it should "return proper result for toleration greater than signal" in {
    val expected = Seq(("2008-01-01T12:35:00Z", "2008-01-01T13:04:00Z")).map(createSession)
    Sessions.findSessions(seriesOfTypicalRain, Duration.ofMinutes(100)) shouldBe expected
  }

  it should "detect no session with toleration of 5 minutes in signal of all zero values " in {
    val expected: Seq[Session] = Seq()
    Sessions.findSessions(seriesOfNoRain, Duration.ofMinutes(5)) shouldBe expected
  }

  it should "split time series by session " in {
    val idx = Vector("2008-01-01T12:37:00Z", "2008-01-01T12:38:00Z", "2008-01-01T12:39:00Z", "2008-01-01T12:40:00Z"
      , "2008-01-01T12:41:00Z", "2008-01-01T12:42:00Z", "2008-01-01T12:43:00Z", "2008-01-01T12:44:00Z"
      , "2008-01-01T12:45:00Z", "2008-01-01T12:46:00Z", "2008-01-01T12:47:00Z", "2008-01-01T12:48:00Z"
      , "2008-01-01T12:49:00Z", "2008-01-01T12:50:00Z", "2008-01-01T12:51:00Z").map(Instant.parse)
    val session = createSession("2008-01-01T12:37:00Z", "2008-01-01T12:51:00Z")
    val vs = Vector(1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    val expected = TimeSeries(idx, vs)

    Sessions.splitBySession(seriesOfEvanescentRain, session) shouldBe expected
  }

}