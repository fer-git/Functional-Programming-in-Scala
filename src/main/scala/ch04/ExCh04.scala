package ch04

/**
  * Created by fernandopratama on 18/5/16.
  */
object ExCh04 {
  /**
    * Compute the mean of a sequence
    * @param xs Sequence operated on
    * @return The mean of xs
    */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
    * Ex02, Implement variance function in terms of mean and flatMap
    * @param xs Sequence operated on
    * @return The variance of xs
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs map (x => math.pow(x - m, 2))))
  }

  /**
    * Ex03, Combine two Option values using a binary function. If either Option
    * value
    * is None then return value is also None
    * @param a first Option
    * @param b second Option
    * @param f function to be applied on values of a and b
    * @return Function application on values of a and b
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      valA <- a
      valB <- b
    } yield f(valA, valB)
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
}
