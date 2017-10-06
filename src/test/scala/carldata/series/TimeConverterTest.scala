package carldata.series

import java.time.LocalDateTime

import carldata.series.TimeConverter._
import org.scalatest._


class TimeConverterTest extends FlatSpec with Matchers {

  "TimeConverter" should "parse cron string */5 * * * *" in {
    val expected = CronLike(RepeatElement(5), AnyElement, AnyElement, AnyElement, AnyElement)
    TimeConverter.mkCronLike("*/5 * * * *").get shouldBe expected
  }

  it should "parse cron string 5 2 * 4 *" in {
    val expected = CronLike(NumberElement(5), NumberElement(2), AnyElement, NumberElement(4), AnyElement)
    TimeConverter.mkCronLike("5 2 * 4 *").get shouldBe expected
  }

  it should "parse cron string * * * * *" in {
    val expected = CronLike(AnyElement, AnyElement, AnyElement, AnyElement, AnyElement)
    TimeConverter.mkCronLike("* * * * *").get shouldBe expected
  }

  it should "parse cron string 5,1,2 2 * 4 *" in {
    val expected = CronLike(ListElement(Seq(5, 1, 2)), NumberElement(2), AnyElement, NumberElement(4), AnyElement)
    TimeConverter.mkCronLike("5,1,2 2 * 4 *").get shouldBe expected
  }

  it should "parse cron string 5a,1,2 2 * 4 *" in {
    val expected = CronLike(ListElement(Seq(5, 1, 2)), NumberElement(2), AnyElement, NumberElement(4), AnyElement)
    TimeConverter.mkCronLike("5a,1,2 2 * 4 *") shouldBe None
  }

  it should "parse cron string 5a 2 * 4 *" in {
    val expected = CronLike(ListElement(Seq(5, 1, 2)), NumberElement(2), AnyElement, NumberElement(4), AnyElement)
    TimeConverter.mkCronLike("5a 2 * 4 *") shouldBe None
  }

  it should "not parse cron string 5,k,2 2 * 4 *" in {
    TimeConverter.mkCronLike("5,k,2 2 * 4 *") shouldBe None
  }

  it should "not parse cron string k 2 * 4 *" in {
    TimeConverter.mkCronLike("k 2 * 4 *") shouldBe None
  }

  it should "convert cron type */5 * * * *" in {
    val cron = CronLike(RepeatElement(5), AnyElement, AnyElement, AnyElement, AnyElement)
    val dt = LocalDateTime.of(2017, 10, 10, 12, 18)
    val expected = LocalDateTime.of(2017, 10, 10, 12, 15)
    TimeConverter.mkConverter(cron).apply(dt) shouldBe expected
  }

  it should "convert cron type 5 * * * *" in {
    val cron = CronLike(NumberElement(5), AnyElement, AnyElement, AnyElement, AnyElement)
    val dt = LocalDateTime.of(2017, 10, 10, 12, 18)
    val expected = LocalDateTime.of(2017, 10, 10, 12, 5)
    TimeConverter.mkConverter(cron).apply(dt) shouldBe expected
  }

  it should "convert cron type 2,3,14 * * * *" in {
    val cron = CronLike(ListElement(Seq(2, 3, 14)), AnyElement, AnyElement, AnyElement, AnyElement)
    val dt = LocalDateTime.of(2017, 10, 10, 12, 7)
    val expected = LocalDateTime.of(2017, 10, 10, 12, 3)
    TimeConverter.mkConverter(cron).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type */5 * * * *" in {
    val dt = LocalDateTime.of(2017, 10, 10, 12, 7)
    val expected = LocalDateTime.of(2017, 10, 10, 12, 5)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("*/5 * * * *").get).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type 2,3,14 * * * *" in {
    val dt = LocalDateTime.of(2017, 10, 10, 12, 7)
    val expected = LocalDateTime.of(2017, 10, 10, 12, 3)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("2,3,14 * * * *").get).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type * 12 * * *" in {
    val dt = LocalDateTime.of(2017, 10, 10, 11, 7)
    val expected = LocalDateTime.of(2017, 10, 9, 12, 59)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("* 12 * * *").get).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type * * * 4 * case 1" in {
    val dt = LocalDateTime.of(2017, 10, 10, 11, 7)
    val expected = LocalDateTime.of(2017, 4, 30, 23, 59)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("* * * 4 *").get).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type * * * 4 * case 2" in {
    val dt = LocalDateTime.of(2017, 4, 10, 11, 7)
    val expected = LocalDateTime.of(2017, 4, 10, 11, 7)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("* * * 4 *").get).apply(dt) shouldBe expected
  }

  it should "parse and convert cron type * * * 4 * case 3" in {
    val dt = LocalDateTime.of(2017, 1, 10, 11, 7)
    val expected = LocalDateTime.of(2016, 4, 30, 23, 59)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("* * * 4 *").get).apply(dt) shouldBe expected
  }

/*  it should "parse and convert cron type * * * * 3" in {
    val dt = LocalDateTime.of(2017, 10, 10, 11, 7)
    val expected = LocalDateTime.of(2016, 4, 30, 23, 59)
    TimeConverter.mkConverter(TimeConverter.mkCronLike("* * * * 3").get).apply(dt) shouldBe expected
  }*/

}