package carldata.series

import java.time.LocalDateTime


object TimeConverter {

  sealed trait CronElement

  case class NumberElement(v: Int) extends CronElement

  case class RepeatElement(v: Int) extends CronElement

  case class ListElement(s: Seq[Int]) extends CronElement

  case class AnyElement() extends CronElement

  case class CronLike(minutes: CronElement, hour: CronElement, dayOfMonth: CronElement, month: CronElement
                      , dayOfWeek: CronElement)

  def mkCronLike(s: String): Either[String, CronLike] = {
    val xs = s.split(" ")
      .map(parseElement)
    if (xs.contains(None) || xs.length != 5)
      Left("Provided cron was incorrect")
    else {
      val ys = xs.map(x => x.get)
      Right(CronLike(ys(0), ys(1), ys(2), ys(3), ys(4)))
    }
  }

  def mkConverter(c: CronLike): LocalDateTime => LocalDateTime = { dt =>
    def floor(x: Int, c: CronElement): Int = {
      c match {
        case n: NumberElement => n.v
        case r: RepeatElement => (x / r.v) * r.v
        case l: ListElement => l.s.sorted.reverse.find(n => n < x).getOrElse(0)
        case a: AnyElement => x
      }
    }

    dt.withMinute(floor(dt.getMinute, c.minutes))
  }

  private def parseAny(s: String): Option[AnyElement] = {
    if (s.length == 1 && s == "*") Some(AnyElement())
    else None
  }

  private def parseList(s: String): Option[ListElement] = {
    if (s.contains(",")) {
      val xs = s.split(",")
      if (xs.nonEmpty) {
        val ys = xs.map(x =>
          if (Character.isDigit(x(0))) Some(x.toInt)
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

  private def parseNumber(s: String): Option[NumberElement] = {
    if (Character.isDigit(s(0)) && !s.contains(",")) Some(NumberElement(s.toInt))
    else None
  }

  private def parseRepetition(s: String): Option[RepeatElement] = {
    if (s.contains("/")) {
      val x = parseNumber(s.split("/")(1))
      if (x.isDefined) Some(RepeatElement(x.get.v))
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

