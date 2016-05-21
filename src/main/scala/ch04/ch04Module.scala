package ch04

import java.util.regex.{PatternSyntaxException, Pattern}

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

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern (pat) map (p => (s: String) => p.matcher(s).matches)
  }

  def mkMatcher_1 (pat: String): Option[String => Boolean] = {
    for {
      p <- pattern(pat)
    } yield (s: String) => p.matcher(s).matches
  }

  def doesMatch(pat: String, s: String): Option[Boolean] = {
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)
  }

  def bothMatches(pat: String, pat2: String, s: String): Option[Boolean] = {
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)
  }

  def bothMatches_1(pat: String, pat2: String, s: String): Option[Boolean] = {
    mkMatcher(pat) flatMap(f => mkMatcher(pat2) map (g => f(s) && g(s)))
  }

  def meanEither(xs: IndexedSeq[Double]):Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = {
    try{
      Right(x/y)
    } catch {
      case e: Exception => Left(e)
    }
  }


  def main(args: Array[String]) {
    println(mean(List(1, 2, 3)))
    println(mean_2(List()))
  }
}
