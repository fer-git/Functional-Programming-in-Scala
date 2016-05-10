package ch02Test

import ch02.ExCh02._
import org.scalatest.{FunSpec, Matchers}
/**
  * Created by fernandopratama on 10/5/16.
  */
class ExCh02Test extends FunSpec with Matchers {
  describe("A fibonacci function, fib") {
    it("Should return 0 for the zeroth index") {
      fib(0) should equal (0)
    }
    it("Should return 1 for the first index") {
      fib(1) should equal (1)
    }
    it("Should return 3 for the fifth index") {
      fib(4) should equal (3)
    }
    it("Should return 5 for the sixth index") {
      fib(5) should equal (5)
    }
  }

  def gt(x: Int, y: Int): Boolean = x > y
  describe("isSorted function") {
    it("Should return true for empty array") {
      isSorted(Array[Int](), gt) should equal (true)
    }
    it("Should return true for single element array") {
      isSorted(Array[Int](2), gt) should equal (true)
    }

    it("Should return true for 3-elements sorted array") {
      isSorted(Array[Int](2, 4, 5), gt) should equal (true)
    }

    it("Should return true for 4-elements sorted array") {
      isSorted(Array[Int](2, 3, 10, 12), gt) should equal (true)
    }

    it("Should return false for non-sorted array") {
      isSorted(Array[Int](3, 10, 1), gt) should equal (false)
    }

    it("Should return false for non-sorted 4-elements array") {
      isSorted(Array[Int](10, 3, 5, 1), gt) should equal (false)
    }
  }

  def sum(x: Int, y: Int): Int = x + y
  describe("partial1 function") {
    it("Should return a function that increment the input by 1") {
      val incBy1 = partial1(1, sum)
      incBy1(10) should equal (11)
    }

    it("Should return a function that decrement the input by 10") {
      val decBy10 = partial1(-10, sum)
      decBy10(20) should equal (10)
    }
  }

  describe("curry function") {
    it ("Should return a function that add a number by ten") {
      val curriedSum = curry(sum)
      val sumByTen = curriedSum(10)
      sumByTen(100) should equal (110)
    }

    it ("Should return a function that subtract a number by 20") {
      val curriedSum = curry(sum)
      val reduceByTwenty = curriedSum(-20)
      reduceByTwenty(20) should equal (0)
    }
  }

  describe("uncurry function") {
    it ("Should return function sum that takes two numbers") {
      val curriedSum = {
        (x: Int) => {
          (y: Int) => x + y
        }
      }
      val uncurriedSum = uncurry(curriedSum)
      uncurriedSum(10, 11) should equal(21)
    }

    it ("Should return function product that takes two numbers") {
      val curriedProduct = {
        (x: Double) => {
          (y: Double) => x * y
        }
      }
      val uncurriedProduct = uncurry(curriedProduct)
      uncurriedProduct(20, 11) should equal (220)
    }
  }

  describe("compose function") {
    it("Should return composed function: firstly add by two, then multiply" +
      " by 3") {
      val addWith2 = (x: Int) => x + 2
      val prodWith3 = (x: Int) => x * 3
      val composedFunction = compose(prodWith3, addWith2)
      composedFunction(20) should equal(66)
    }

    it("Should return composed function: firstly multiply by 3, then add by 2") {

    }
    val addWith2 = (x: Int) => x + 2
    val prodWith3 = (x: Int) => x * 3
    val composedFunction = compose(addWith2, prodWith3)
    composedFunction(20) should equal(62)
  }
}