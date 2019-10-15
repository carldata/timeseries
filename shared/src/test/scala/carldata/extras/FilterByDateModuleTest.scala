package carldata.extras

import java.time.Instant

import carldata.extras.FilterByDateModule._
import carldata.series.TimeSeries
import org.scalatest.{FlatSpec, Matchers}

class FilterByDateModuleTest extends FlatSpec with Matchers {
  "FilterByDateModule" should "get last for each minute" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:35:15Z"
      , "2019-10-01T12:35:44Z", "2019-10-01T12:36:20Z", "2019-10-01T12:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T12:35:00Z", "2019-10-01T12:36:00Z").map(Instant.parse(_)), Vector(3, 5))

    series.getLastOfTheMinute shouldBe expected
  }

  it should "get last for each hour" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:36:15Z"
      , "2019-10-01T12:37:44Z", "2019-10-01T13:36:20Z", "2019-10-01T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T12:00:00Z", "2019-10-01T13:00:00Z").map(Instant.parse(_)), Vector(3, 5))

    series.getLastOfTheHour shouldBe expected
  }

  it should "get last for each day" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:36:15Z"
      , "2019-10-01T12:37:44Z", "2019-10-02T13:36:20Z", "2019-10-02T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T00:00:00Z", "2019-10-02T00:00:00Z").map(Instant.parse(_)), Vector(3, 5))

    series.getLastOfTheDay shouldBe expected
  }

  it should "get last for each month" in {
    val idx = Vector("2019-10-15T12:35:01Z", "2019-10-16T12:36:15Z"
      , "2019-10-17T12:37:44Z", "2019-11-23T13:36:20Z", "2019-11-24T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T00:00:00Z", "2019-11-01T00:00:00Z").map(Instant.parse(_)), Vector(3, 5))

    series.getLastOfTheMonth() shouldBe expected
  }

  it should "get last for each year" in {
    val idx = Vector("2018-10-15T12:35:01Z", "2018-10-16T12:36:15Z"
      , "2018-10-17T12:37:44Z", "2019-11-23T13:36:20Z", "2019-11-24T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2018-01-01T00:00:00Z", "2019-01-01T00:00:00Z").map(Instant.parse(_)), Vector(3, 5))

    series.getLastOfTheYear() shouldBe expected
  }

  it should "get First for each minute" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:35:15Z"
      , "2019-10-01T12:35:44Z", "2019-10-01T12:36:20Z", "2019-10-01T12:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T12:35:00Z", "2019-10-01T12:36:00Z").map(Instant.parse(_)), Vector(1, 4))

    series.getFirstOfTheMinute shouldBe expected
  }

  it should "get First for each hour" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:36:15Z"
      , "2019-10-01T12:37:44Z", "2019-10-01T13:36:20Z", "2019-10-01T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T12:00:00Z", "2019-10-01T13:00:00Z").map(Instant.parse(_)), Vector(1, 4))

    series.getFirstOfTheHour shouldBe expected
  }

  it should "get First for each day" in {
    val idx = Vector("2019-10-01T12:35:01Z", "2019-10-01T12:36:15Z"
      , "2019-10-01T12:37:44Z", "2019-10-02T13:36:20Z", "2019-10-02T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T00:00:00Z", "2019-10-02T00:00:00Z").map(Instant.parse(_)), Vector(1, 4))

    series.getFirstOfTheDay shouldBe expected
  }

  it should "get First for each month" in {
    val idx = Vector("2019-10-15T12:35:01Z", "2019-10-16T12:36:15Z"
      , "2019-10-17T12:37:44Z", "2019-11-23T13:36:20Z", "2019-11-24T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2019-10-01T00:00:00Z", "2019-11-01T00:00:00Z").map(Instant.parse(_)), Vector(1, 4))

    series.getFirstOfTheMonth() shouldBe expected
  }

  it should "get First for each year" in {
    val idx = Vector("2018-10-15T12:35:01Z", "2018-10-16T12:36:15Z"
      , "2018-10-17T12:37:44Z", "2019-11-23T13:36:20Z", "2019-11-24T13:36:21Z").map(Instant.parse(_))
    val series = TimeSeries(idx, Vector(1, 2, 3, 4, 5))
    val expected = TimeSeries(Vector("2018-01-01T00:00:00Z", "2019-01-01T00:00:00Z").map(Instant.parse(_)), Vector(1, 4))

    series.getFirstOfTheYear() shouldBe expected
  }
}
