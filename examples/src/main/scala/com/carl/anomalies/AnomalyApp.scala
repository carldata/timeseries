package com.carl.anomalies

import com.carl.ts.Series


/**
  * Created by Krzysztof Langner on 2017-03-28.
  */
object AnomalyApp {

  def main(args: Array[String]): Unit = {
    val series = Series.empty
    println("Hello from anomaly detection: " + series)
  }
}
