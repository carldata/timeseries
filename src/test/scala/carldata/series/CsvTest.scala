package carldata.series

import org.scalatest._


class CsvTest extends FlatSpec with Matchers {

  "Csv" should "read series from string with csv values" in {
    val str =
      """time,value
        |2005-01-01T12:34:15,2
        |2006-01-01T12:34:15,-4
        |2007-01-01T12:34:15,-6
        |2008-01-01T12:34:15,9""".stripMargin
    val series = Csv.fromString(str)
    series.values shouldBe Vector(2, -4, -6, 9)
  }

}