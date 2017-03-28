package com.carl.ts.examples

import java.io.File
import java.time.LocalDateTime

import com.github.tototoshi.csv.CSVReader
import com.carl.ts.Series
import com.carl.ts.Stats.SeriesStats


/**
  * Created by Krzysztof Langner on 2017-03-28.
  */
object AnomalyApp {

  def main(args: Array[String]): Unit = {
    val series = loadCSV("testdata/anomalies.csv")
    printStats("Raw data", series)
    printStats("Differences", series.differentiate)
  }

  def loadCSV(fileName: String): Series[Double] = {
    import java.time.format.DateTimeFormatter
    val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val reader = CSVReader.open(new File(fileName))
    val data = reader.allWithHeaders()
    val readings: List[(LocalDateTime, Double)] = data.map(row => {
      val index = row.get("time").map(x => LocalDateTime.parse(x,format)).getOrElse(LocalDateTime.now())
      val value = row.get("velocity").map(_.toDouble).getOrElse(0.0)
      (index, value)
    })
    new Series(readings)
  }

  def printStats(title: String, series: Series[Double]) = {
    println(title)
    println("  * Length: " + series.length)
    println("  * Mean: " + series.mean)
    println("  * Std dev: " + series.stddev)

  }
}
