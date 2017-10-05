package carldata.series

sealed trait CronElement

case class NumberElement(v: Int) extends CronElement

case class RepeatElement(v: Int) extends CronElement

case class ListElement(s: Seq[Int]) extends CronElement

case class AnyElement() extends CronElement

case class CronLike(minutes: CronElement, hour: CronElement, dayOfMonth: CronElement, month: CronElement
                    , dayOfWeek: CronElement)

object TimeConverter {

  def mkCronLike(s: String): Either[String, CronLike] = {
    val xs = s.split(" ")
      .map(x => (isNumber(x), x))
      .map(x => if (x._1.isEmpty) (isRepetition(x._2), x._2) else x)
      .map(x => if (x._1.isEmpty) (isList(x._2), x._2) else x)
      .map(x => if (x._1.isEmpty) (isAny(x._2), x._2) else x)
    if (xs.map(x => x._1).contains(None))
      Left("Provided cron was incorrect")
    else {
      val ys = xs.map(x => x._1.get)
      Right(CronLike(ys(0), ys(1), ys(2), ys(3), ys(4)))
    }
  }

  def mkConverter() = {}

  private def isAny(s: String): Option[AnyElement] = {
    if (s.length == 1 && s == "*") Some(AnyElement())
    else None
  }

  private def isList(s: String): Option[ListElement] = {
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

  private def isNumber(s: String): Option[NumberElement] = {
    if (Character.isDigit(s(0)) && !s.contains(",")) Some(NumberElement(s.toInt))
    else None
  }

  private def isRepetition(s: String): Option[RepeatElement] = {
    if (s.contains("/")) {
      val x = isNumber(s.split("/")(1))
      if (x.isDefined) Some(RepeatElement(x.get.v))
      else None
    }
    else None
  }


}

