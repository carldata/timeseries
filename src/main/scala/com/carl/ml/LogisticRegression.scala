package com.carl.ml

object LogisticRegression {
  /** Build model from training and target data. */
  def fit(train: Seq[Vector[Double]], target: Seq[Boolean]): LogisticRegression = {
    new LogisticRegression(Vector.empty)
  }
}

class LogisticRegression (model: Vector[Double]){

  /** Predict class labels for given samples.*/
  def predict(samples: Seq[Vector[Double]]): Seq[Boolean] = {
    Seq.empty
  }
}
