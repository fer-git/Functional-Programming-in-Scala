package ch05

import Stream._

/**
  * Created by fernandopratama on 21/5/16.
  */
sealed trait Stream[+A] {
  /**
    * Ex01, Write a function to convert a Stream to a List
    * @return
    */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /**
    * Ex02, Take the first n elements of a Stream
    * @param n The number elements needs to be taken
    * @return The first n elements of a Stream
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /**
    * Ex03, returning all starting elements of a Stream that matches the
    * given predicate
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * Ex04, Implement forAll, which checks that all element in the Stream
    * match a
    * given predicate
    * @param f Function to be applied on each element of the Stream
    * @return True if predicate of each element is true, else false
    */
  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  /**
    * Ex05, use foldRight to implement takeWhile
    * @param p Predicate function to be appleid on each element of the Stream
    * @return Starting elements in which the predicate of the element is true
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
    if (p(h)) cons(h, t)
    else empty)
  }

  /*
  Ex06, Implement map, filter, append and flatMap using foldRight
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B>:A](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  /**
    * EX07, Return an infinite stream of a given values
    * @param a The element in the Stream
    * @return Stream with element a
    */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
    * Ex08, Generate infinite stream of integers
    * @param n The starting value of the Stream
    * @return Stream with elements started with n, and incremented by 1 on
    *         the next element
    */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
    * Ex09, Generate the infinite stream of Fibonacci number
    */
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }

  /**
    * Ex10, General stream building function. Take an initial state, and
    * function for producing both the next value and state in the
    * generated stream
    * @param z Initial State
    * @param f Function to produce next value and state in generated
    *          stream
    * @return Stream generated with initial value z, and function f
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }
}