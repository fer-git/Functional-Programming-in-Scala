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

  /**
    * Ex12 Function that return the reverse of a list with fold
    * @param l List operated on
    * @return List with reversed element
    */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z: List[A], x: A) => Cons(x, z))

  /**
    * Ex13 Define foldLeft in terms of foldRight
    * @param l List operated on
    * @param z Accumulator
    * @param f Function that accepts two parameters, current accumulator and
    *          an element of the l
    * @return Reduced operation on l with initial accumulator z and function f
    */
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x: A, acc: B) => f(acc, x))

  /**
    * Ex14, Append two lists into a list via foldLeft
    * @param l First list
    * @param xs Second list
    * @return List with elements from l and xs
    */
  def appendFL[A](l: List[A], xs: List[A]): List[A] =
    foldLeft(reverse(l), xs)((acc: List[A], x: A) => Cons(x, acc))

  /**
    * Ex14, Append two list into a list via foldRight
    * @param l First list
    * @param xs Second list
    * @return List with elements from l and xs
    */
  def appendFR[A](l: List[A], xs: List[A]): List[A] =
    foldRight(l, xs)(Cons(_, _))

  /**
    * Ex15, Concatenates a list of lists into a single list
    * @param ll List of list operated on
    * @return A list with elements from all list of list
    */
  def flatMap[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, Nil: List[A])((acc: List[A], xs: List[A]) => appendFL(acc, xs))

  /**
    * Ex16, Transform a list of integers by adding 1 to each element
    * @param l List operated on
    * @return Integer list with each element incremented by 1
    */
  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, add1(xs))
  }

  /**
    * Ex17, Turn each value in List[Double] into String
    * @param l List operated on
    * @return List with elements casted into string
    */
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  /**
    * Ex18, Modify each element of list with a function f
    * @param l List operated on
    * @param f Function that accepts a parameter, to be applied on each
    *          list's element
    * @return List with each element applied with function f
    */
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  /**
    * Ex19, Remove element from a list unless they satisfy a given predicate
    * @param l List operated on
    * @param f Predicate function for each element in the list
    * @return list with filtered element by predicate f
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) if !f(x) => filter(xs)(f)
  }

  /**
    * Ex20, Higher order function works like map except the function given
    * will return a list instead of a single result, the list should be
    * inserted into the final resulting list
    * @param l List operated on
    * @param f Function that accepts one parameter, to be applied on each
    *          list's element, returns a list
    * @return Return a list of list consists of each element applied by f
    */
  def flatMap1[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => appendFL(f(x), flatMap1(xs)(f))
  }

  /**
    * Concatenate list of list into a single list
    * @param ll List of list operated on
    * @return List consists of each elements on each original list
    */
  def concat[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil: List[A])(appendFL)

  /**
    * Alternative implementation of flatMap1 with foldRight
    * @param l List operated on
    * @param f Function that accepts one parameter, to be applied on each
    *          list's element, returns a list
    * @return Return a list of list consists of each element applied by f
    */
  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  /**
    * Ex21, filter implementation via flatMap
    * @param l List operated on
    * @param f Predicate function for each element in the list
    * @return list with filtered element by predicate f
    */
  def filterViaflatMap1[A](l: List[A])(f: A => Boolean): List[A] =
  flatMap1(l)((x: A) => if (f(x)) List(x) else Nil)

  /**
    * Ex22, Function that accepts two integer lists that adds them element-wise
    * @param l First list
    * @param xs Second list
    * @return List where is element is by adding corresponding elements from
    *         the two input lists
    */
  def addList(l: List[Int], xs: List[Int]): List[Int] = (l, xs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(y, ys)) => Cons(h + y, addList(t, ys))
  }

  /**
    * Ex23, Generalization of addList, accept a function to be applied on
    * each element pair from two input list
    * @param l First list
    * @param xs Second list
    * @param f Function that accepts two parameters, first parameter is for
    *          element in first list, and second parameter is for element in
    *          second list
    * @return List that consists of application of corresponding elements
    *         from each input list
    */
  def zipWith[A, B, C](l: List[A], xs: List[B])(f: (A, B) => C): List[C] =
    (l, xs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h, t), Cons(y, ys)) => Cons(f(h, y), zipWith(t, ys)(f))
    }

  /**
    * Ex24, Check whether a List contains another List as subsequence
    * @param l List operated on
    * @param sub List in which we check whether it is subsequence of l or not
    * @return True is sub is subsequence of l, false otherwise
    */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(x, xs) => hasSubsequence(xs, sub)
  }

  /**
    * Check if the second list shares the same starting elements with the first
    * list
    * @param l First list
    * @param xs Second list
    * @return True if xs shares the same starting elements with l, false
    *         otherwise
    */
  def startsWith[A](l: List[A], xs: List[A]): Boolean = (l, xs) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(y, ys)) if h == y => startsWith(t, ys)
    case _ => false
  }
}
