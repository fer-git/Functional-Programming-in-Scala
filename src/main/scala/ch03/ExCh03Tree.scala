package ch03

/**
  * Created by fernandopratama on 17/5/16.
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
    * Ex25, Count the number of nodes in a tree
    * @param t Tree operated on
    * @return The number of nodes in t
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
  }

  /**
    * Ex26, Get the maximum element in a Tree[Int]
    * @param t Tree operated on
    * @return Maximum element in t
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(t1, t2) => maximum(t1) max maximum(t2)
  }

  /**
    * Ex27, Get the maximum path length from the root of a tree to any leaf
    * @param t Tree operated on
    * @return The maximum path length from root to leaf
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(t1, t2) => 1 + depth(t1) max depth(t2)
  }

  /**
    * Ex28, Apply function on each leaf in tree
    * @param t Tree operated on
    * @param f Function accepts one parameter, to be applied on each leaf in t
    * @return Tree in which each element has been applied by f
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
  }

  /*
  Ex29, generalize size, maximum, depth, and map writing a new function fold
  that abstracts over their similarities.
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g:(B, B) => B): B = ???

  def sizeViaFold[A](t: Tree[A]): Int = ???

  def maximumViaFold(t: Tree[Int]): Int = ???

  def depthViaFold[A](t: Tree[A]): Int = ???

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
}
