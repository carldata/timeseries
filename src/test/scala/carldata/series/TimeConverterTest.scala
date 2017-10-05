package carldata.series

import java.time.{Duration, LocalDateTime, ZoneOffset}

import org.scalatest._


class TimeConverterTest extends FlatSpec with Matchers {

  "TimeConverter" should "parse cron string */5 * * * *" in {
    val expected = CronLike(RepeatElement(5), AnyElement(), AnyElement(), AnyElement(), AnyElement())
    TimeConverter.mkCronLike("*/5 * * * *").right.get shouldBe expected
  }

  it should "parse cron string 5 2 * 4 *" in {
    val expected = CronLike(NumberElement(5), NumberElement(2), AnyElement(), NumberElement(4), AnyElement())
    TimeConverter.mkCronLike("5 2 * 4 *").right.get shouldBe expected
  }

  it should "parse cron string 5,1,2 2 * 4 *" in {
    val expected = CronLike(ListElement(Seq(5, 1, 2)), NumberElement(2), AnyElement(), NumberElement(4), AnyElement())
    TimeConverter.mkCronLike("5,1,2 2 * 4 *").right.get shouldBe expected
  }

  it should "not parse cron string 5,k,2 2 * 4 *" in {
    TimeConverter.mkCronLike("5,k,2 2 * 4 *") shouldBe Left("Provided cron was incorrect")
  }

  it should "not parse cron string k 2 * 4 *" in {
    TimeConverter.mkCronLike("k 2 * 4 *") shouldBe Left("Provided cron was incorrect")
  }

}