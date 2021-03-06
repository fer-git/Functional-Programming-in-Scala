package ch04

import java.util.regex.{PatternSyntaxException, Pattern}

/**
  * Created by fernandopratama on 18/5/16.
  */
object ExCh04 {
  /**
    * Compute the mean of a sequence
    *
    * @param xs Sequence operated on
    * @return The mean of xs
    */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /**
    * Ex02, Implement variance function in terms of mean and flatMap
    *
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
    *
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

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    for{
      p <- pattern(pat)
    } yield (s: String) => p.matcher(s).matches
  }

  def doesMatch(pat: String, s: String): Option[Boolean] = {
    for {
      p <- mkMatcher(pat)
    } yield p(s)
  }

  /**
    * Ex04, Reimplement bothMatch in terms of map2 function
    *
    * @param pat1 First pattern
    * @param pat2 Second Pattern
    * @param s String to be matched by pat1 and pat2
    * @return True if both pattern match the string s, else false
    */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(doesMatch(pat1, s), doesMatch(pat2, s))((x, y) => x && y)

  /**
    * Ex05, Combine a list of Options into one option containing a list of
    * all the Some values in the original list
    *
    * @param a List of Option
    * @return List of Some from original list, if the original list contains
    *         at least one None, then the return is None
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  /**
    * Ex06, Combination of map and sequence, but with only looks at the list
    * once.
    * @param a List operated on
    * @param f Function to be applied on each element
    * @return Option of List which f has been applied on each element on
    *         original list
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  /**
    * Ex08, implement traverse for Either
    * @param es List operated on
    * @param f Function to be applied on each element on es
    * @return Return Left if at least application f on element of es results
    *         in Left, else Right with each element applied by f
    */
  def traverseEither[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E,
    List[B]] = es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a)
    .map2(b)(_ :: _))

  /**
    * Ex08, implement sequence for Either
    * @param es List operated on
    * @return Return Left if at least one of the element in es is Left, else
    *         return a List with the content of each Right of es
    */
  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverseEither(es)(x => x)
  }
}
