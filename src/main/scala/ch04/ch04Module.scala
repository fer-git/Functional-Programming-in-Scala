package ch04

/**
  * Created by fernandopratama on 17/5/16.
  */

object ch04Module {
  def mean(xs: Seq[Double]): Double = {
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list")
    else xs.sum / xs.length
  }

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = {
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
  }

  def mean_2(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def main(args: Array[String]) {
    println(mean(List(1, 2, 3)))
    println(mean_2(List()))
  }
}
