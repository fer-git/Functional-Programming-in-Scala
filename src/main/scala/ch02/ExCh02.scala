package ch02

/**
  * Created by fernandopratama on 10/5/16.
  */
object ExCh02 {
  /**
    * Ex01, Return the nth fibonacci number, index start from 0
    * @param n The nth position
    * @return Nth fibonacci number
    */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = n match {
      case 0 => a
      case _ => go(b, a+b, n-1)
    }
    go(0, 1, n)
  }

  /**
    * Ex02, Check if an array is sorted from smallest to biggest, return boolean
    * value
    * @param as The array that we want to check
    * @param gt A function that return true if the first one is greater than the
    *           second one, and vice versa
    * @return True if array is sorted, false otherwise
    */
  def isSorted[A](as: Array[A], gt:(A, A) => Boolean): Boolean = {
    def go(curArray: Array[A], acc: Boolean): Boolean = {
      if (curArray.length == 0 | curArray.length == 1) acc
      else go(curArray.tail, !gt(curArray(0), curArray(1)) && acc)
    }
    go(as, acc = true)
  }

  /**
    * Ex03, Apply value on the first function's parameter.
    * @param a The value of first parameter
    * @param f The function that accepts two parameters, the first parameter
    *          type matches with param a
    * @return Partially applied function on the first parameter
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /**
    * Ex04, Convert a function that takes 2 arguments into a function of one
    * argument that returns another function as its result.
    * @param f Function that takes two arguments
    * @return Function that takes one argument, which is the same first argument
    *         of original function, and return a function that accepts
    *         original function's second argument that returns the same
    *         result as original function
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a, b)
    }
  }

  /**
    *  Ex05, Reverse the transformation of curry
    * @param f A function that takes one argument, and return a function that
    *          takes another argument
    * @return A function which takes two arguments and return the same type as f
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => {
      f(a)(b)
    }
  }

  /**
    * Ex06, Higher order function that composes two functions
    * @param f The first function on composition
    * @param g The second function on composition
    * @return The composed function of g o f
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}