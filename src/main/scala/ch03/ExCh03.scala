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

  /**
    * Ex03, Drop n many elements from a list
    * @param lst List operated on
    * @param n Number of elements need to be dropped from the beginning
    *          of the list
    * @return List with dropped n elements
    */
  def drop[A](lst: List[A], n: Int): List[A] = (lst, n) match {
    case (Nil, _) => Nil
    case (xs, k) if k <= 0 => xs
    case (Cons(x, xs), k) => drop(xs, k - 1)
  }

  /**
    * Ex04, Removes elements from the List prefix as long as they match a
    * predicate
    * @param lst List operated on
    * @param f Predicate function for each element
    * @return The remaining elements of a list where the first element is the
    *         first time the predicate function on that element is false
    */
  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case list @ Cons(x, xs) if !f(x) => list
    case Cons(_, xs) => dropWhile(xs)(f)
  }

  /**
    * Ex05, Replace the first element of a List with different value
    * @param lst List operated on
    * @param x Replacement on the first element
    * @return The list with first element replaced with parameter x
    */
  def setHead[A](lst: List[A], x: A): List[A] = lst match {
    case Nil => Nil
    case Cons(h, t) => Cons(x, t)
  }

  /**
    * Ex06, return a List consisting of all but the last element of a list
    * @param lst List operated on
    * @return The same as original list except the last element
    */
  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0)(_ + _)

  def product2(l: List[Double]) = foldRight(l, 1.0) (_ * _)

  /**
    * Ex09, Compute the length of a list using foldRight
    * @param l list operated on
    * @return The length of the list
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((x: A, z: Int) => z + 1)

  /**
    * Ex10, Alternative to foldRight, consume the list from left to right
    * @param l List operated on
    * @param z Accumulator
    * @param f Function that accepts two parameters, the first one is for
    *          accumulator, the second one is for the current element on list
    * @return Reduced operation on a list l with function f and initial
    *         accumulator z
    */
  def foldLeft[A, B](l: List[A], z: B)(f:(B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
  Ex11, rewrite sum, product, and length using foldLeft
   */
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((z: Int, x: A) => z + 1)

  def reverse[A](l: List[A]): List[A] = ???

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def appendFL[A](l: List[A], xs: List[A]): List[A] = ???

  def appendFR[A](l: List[A], xs: List[A]): List[A] = ???

  def flatMap[A](ll: List[List[A]]): List[A] = ???
}
