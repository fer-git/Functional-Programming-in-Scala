package ch03

/**
  * Created by fernandopratama on 11/5/16.
  */

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Ex02, Remove the first element on a List
    * @param lst the list
    * @return A List with removed first element from original list
    */
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def drop[A](lst: List[A], n: Int): List[A] = ???

  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = ???

  def setHead[A](lst: List[A], x: A): List[A] = ???
}
