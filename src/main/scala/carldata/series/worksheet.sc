import java.time.Duration

import carldata.series.{Csv, Sessions, TimeSeries}

val series = Csv.fromString("""time,value
                            |2008-01-01T12:35:00, 4
                            |2008-01-01T12:36:00, 2
                            |2008-01-01T12:37:00, 0
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
                            |2008-01-01T12:56:00, 1
                            |2008-01-01T12:57:00, 1
                            |2008-01-01T12:58:00, 2""".stripMargin)


//val str =
//  """time,value
//    |2008-01-01T12:35:00, 4
//    |2008-01-01T12:36:00, 2
//    |2008-01-01T12:37:00, 0
//    |2008-01-01T12:38:00, 0
//    |2008-01-01T12:39:00, 0
//    |2008-01-01T12:40:00, 0
//    |2008-01-01T12:41:00, 0
//    |2008-01-01T12:42:00, 1
//    |2008-01-01T12:43:00, 2
//    |2008-01-01T12:44:00, 0
//    |2008-01-01T12:45:00, 1
//    |2008-01-01T12:46:00, 2
//    |2008-01-01T12:47:00, 3
//    |2008-01-01T12:48:00, 2
//    |2008-01-01T12:49:00, 5
//    |2008-01-01T12:50:00, 6
//    |2008-01-01T12:51:00, 7
//    |2008-01-01T12:52:00, 1
//    |2008-01-01T12:53:00, 0
//    |2008-01-01T12:54:00, 0
//    |2008-01-01T12:55:00, 0
//    |2008-01-01T12:56:00, 1
//    |2008-01-01T12:57:00, 1
//    |2008-01-01T12:58:00, 2""".stripMargin
//val series = Csv.fromString(str)

val rollingWindow = Duration.ofMinutes(3)
//series.slice()

val seriesRolled = series.rollingWindow(rollingWindow, s => s.sum)
seriesRolled.dataPoints


seriesRolled.dataPoints.foldLeft[List[TimeSeries[Double]]] (List())((z, s) => {
  seriesRolled.slice(s._1, s._1.plusMinutes(rollingWindow.toMinutes).plusNanos(1)) :: z
}).reverse

val result = seriesRolled.map(x => {
  if (seriesRolled.slice(x._1, x._1.plusMinutes(rollingWindow.toMinutes).plusNanos(1)).values.contains(0)) 0 else x._2
})

Sessions.findSessions(result)




//series.rollingWindow(Duration.ofMinutes(2), s => s.sum).dataPoints

