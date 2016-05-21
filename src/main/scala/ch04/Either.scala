package ch04

/**
  * Created by fernandopratama on 20/5/16.
  */

sealed trait Either[+E, +A] {
  /*
  Ex07, implement versions of map, flatMap, orElse and map2 on Either that
  operate on the Right value
   */

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
