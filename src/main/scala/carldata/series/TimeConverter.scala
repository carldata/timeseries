package carldata.series

import java.time.LocalDateTime
import java.time.temporal.{TemporalAdjuster, TemporalAdjusters}


object TimeConverter {

  sealed trait CronElement

  case class NumberElement(v: Int) extends CronElement

  case class RepeatElement(v: Int) extends CronElement

  case class ListElement(s: Seq[Int]) extends CronElement

  case object AnyElement extends CronElement

  case class CronLike(minutes: CronElement, hour: CronElement, dayOfMonth: CronElement, month: CronElement
                      , dayOfWeek: CronElement)

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

  def mkConverter(c: CronLike): LocalDateTime => LocalDateTime = { dt =>
    def floor(x: Int, c: CronElement): Int = {
      c match {
        case n: NumberElement => if (x >= n.v) n.v else -(n.v)
        case r: RepeatElement => (x / r.v) * r.v
        case l: ListElement => l.s.sorted.reverse.find(n => n < x).getOrElse(0)
        case _ => x
      }
    }

    val t: Seq[Int] = Seq(
      floor(dt.getMinute, c.minutes)
      , floor(dt.getHour, c.hour)
      , floor(dt.getDayOfMonth, c.dayOfMonth)
      , floor(dt.getMonthValue, c.month)
      , floor(dt.getDayOfWeek.getValue, c.dayOfWeek)
    )
    var res = if (t(0) > 0) dt.withMinute(t(0)) else dt.minusHours(1).withMinute(t(0))
    res = if (t(1) > 0) res.withHour(t(1)) else res.minusDays(1).withHour(Math.abs(t(1))).withMinute(59)
    res = if (t(3) > 0) {
      if (t(3) == dt.getMonthValue) res.withMonth(t(3))
      else res.withMonth(Math.abs(t(3))).`with`(TemporalAdjusters.lastDayOfMonth()).withHour(23).withMinute(59)
    } else res.minusYears(1).withMonth(Math.abs(t(3))).`with`(TemporalAdjusters.lastDayOfMonth()).withHour(23).withMinute(59)

    res
  }

  private def parseAny(s: String): Option[CronElement] = {
    if (s.length == 1 && s == "*") Some(AnyElement)
    else None
  }

  private def parseList(s: String): Option[CronElement] = {
    if (s.contains(",")) {
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

