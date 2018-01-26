package carldata.series

import java.time.LocalDateTime
import java.time.temporal.TemporalAdjusters

/**
  * This module contains functions which convert cron like string into
  * function f: LocalDateTime => LocalDateTime.
  *
  * Example expression: "k 2 * 4 *"
  */
object TimeConverter {

  sealed trait CronElement

  case class NumberElement(v: Int) extends CronElement

  case class RepeatElement(v: Int) extends CronElement

  case class ListElement(s: Seq[Int]) extends CronElement

  case object AnyElement extends CronElement

  case class CronLike(minutes: CronElement, hour: CronElement, dayOfMonth: CronElement, month: CronElement
                      , dayOfWeek: CronElement)

  /** Parse expression */
  def mkCronLike(s: String): Option[CronLike] = {
    val xs = s.split(" ")
      .map(parseElement)
    if (xs.contains(None) || xs.length != 5)
      None
    else {
      val ys = xs.map(x => x.get)
      Some(CronLike(ys(0), ys(1), ys(2), ys(3), ys(4)))
    }
  }

  /** Convert expression into date conversion function */
  def mkConverter(c: CronLike): LocalDateTime => LocalDateTime = { dt =>
    def floor(x: Int, c: CronElement): Int = {
      c match {
        case n: NumberElement => if (x >= n.v) n.v else -n.v
        case r: RepeatElement => ((x + r.v) / r.v) * r.v
        case l: ListElement => l.s.sorted.reverse.find(n => n < x).getOrElse(0)
        case _ => x
      }
    }

    def setDayOfWeek(t1: Int, ct2: CronElement, dt: LocalDateTime): LocalDateTime = {
      ct2 match {
        case nt2: NumberElement =>
          val t2 = nt2.v
          if (t1 == t2) dt
          else if (t1 > t2) dt.minusDays(t1 - t2)
          else dt.minusDays(7 - (t2 - t1))
        case rt2: RepeatElement => setDayOfWeek(t1, NumberElement(rt2.v), dt)
        case _ => dt
      }
    }

    val t: Seq[Int] = Seq(
      floor(dt.getMinute, c.minutes)
      , floor(dt.getHour, c.hour)
      , floor(dt.getDayOfMonth, c.dayOfMonth)
      , floor(dt.getMonthValue, c.month)
      , floor(dt.getDayOfWeek.getValue, c.dayOfWeek)
    )

    var res = if (c.month != AnyElement) {
      if (c.month.isInstanceOf[RepeatElement]) dt.withMonth(t(3)).withDayOfMonth(1).withHour(0).withMinute(0)
      else if (t(3) > 0) {
        if (t(3) == dt.getMonthValue) dt.withMonth(t(3))
        else dt.withMonth(Math.abs(t(3))).`with`(TemporalAdjusters.lastDayOfMonth()).withHour(23).withMinute(59)
      } else dt.minusYears(1).withMonth(Math.abs(t(3))).`with`(TemporalAdjusters.lastDayOfMonth()).withHour(23).withMinute(59)
    } else dt

    if (c.dayOfMonth != AnyElement) {
      res = if (c.dayOfMonth.isInstanceOf[RepeatElement]) res.withDayOfMonth(t(2)).withHour(0).withMinute(0)
      else {
        if (t(2) > 0) {
          if (t(2) == dt.getDayOfMonth) res.withDayOfMonth(t(2))
          else res.withDayOfMonth(Math.abs(t(2))).withHour(23).withMinute(59)
        } else res.minusMonths(1).withDayOfMonth(Math.abs(t(2))).withHour(23).withMinute(59)
      }
    }

    if (c.dayOfWeek != AnyElement) {
      res = if (c.dayOfWeek.isInstanceOf[RepeatElement]) setDayOfWeek(t(4), c.dayOfWeek, res).withHour(0).withMinute(0)
      else {
        if (t(4) > 0) {
          if (t(4) == dt.getDayOfWeek.getValue) res
          else setDayOfWeek(dt.getDayOfWeek.getValue, c.dayOfWeek, res).withHour(23).withMinute(59)
        } else setDayOfWeek(dt.getDayOfWeek.getValue, c.dayOfWeek, res).withHour(23).withMinute(59)
      }
    }

    if (c.hour != AnyElement) {
      res = if (c.hour.isInstanceOf[RepeatElement]) res.withHour(t(1)).withMinute(0)
      else {
        if (t(1) > 0) {
          if (t(1) == dt.getHour) res
          else res.withHour(t(1)).withMinute(59)
        }
        else if (t(1) == 0) res
        else res.minusDays(1).withHour(Math.abs(t(1))).withMinute(59)
      }
    }

    if (c.minutes != AnyElement) {
      res = if (t.head > 0) {
        if (t.head == dt.getMinute) res
        else res.withMinute(t.head)
      } else if (t.head == 0) res
      else res.minusHours(1).withMinute(Math.abs(t.head))
    }

    res.withSecond(0)
      .withNano(0)
  }

  private def parseAny(s: String): Option[CronElement] = {
    if (s == "*") Some(AnyElement)
    else None
  }

  private def parseList(s: String): Option[CronElement] = {
    val xs = s.split(",")
    if (xs.nonEmpty) {
      val ys = xs.map(x =>
        if (x.forall(y => Character.isDigit(y))) Some(x.toInt)
        else None
      )
      if (ys.contains(None)) None
      else
        Some(ListElement(ys.flatten))
    }
    else None
  }

  private def parseNumber(s: String): Option[CronElement] = {
    if (s.forall(x => Character.isDigit(x)) && !s.contains(",")) Some(NumberElement(s.toInt))
    else None
  }

  private def parseRepetition(s: String): Option[CronElement] = {
    if (s.contains("/")) {
      val x = parseNumber(s.split("/")(1))
      if (x.isDefined) Some(RepeatElement(x.get.asInstanceOf[NumberElement].v))
      else None
    }
    else None
  }

  private def parseElement(s: String): Option[CronElement] = {
    Seq(parseNumber(s), parseRepetition(s), parseList(s), parseAny(s))
      .find(x => x.isDefined)
      .flatten
  }


}

