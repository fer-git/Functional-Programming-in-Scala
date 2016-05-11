package ch03Test

import org.scalatest.{Matchers, FunSpec}
import ch03._
/**
  * Created by fernandopratama on 11/5/16.
  */
class ExCh03Test extends FunSpec with Matchers {
  val emptyList = Nil
  val intList = List[Int](1, 2, 3)
  val charList = List[Char]('a', 'b', 'c')

  describe("tail method on List") {
    it("Should return Nil for empty list") {
      List.tail(emptyList) should equal (Nil)
    }

    it("Should return list with the same member as original list except the " +
      "first element on integer list") {
      List.tail(intList) should equal (List[Int](2, 3))
    }

    it("Should return list with the same member as original list except the " +
      "first element on character list") {
      List.tail(charList) should equal (List[Char]('b', 'c'))
    }
  }

  describe("drop method on List") {
    it("Should return empty list for dropping elements of empty list") {
      List.drop(emptyList, 2) should equal(emptyList)
    }

    it("Should behave like tail when dropping one element") {
      List.drop(intList, 1) should equal (List[Int](2, 3))
    }

    it("Should drop 2 elements if n = 2") {
      List.drop(charList, 2) should equal (List[Char]('c'))
    }
  }

  def alwaysTrue[A](x: A): Boolean = true
  def isEven(x: Int): Boolean = (x % 2) == 0

  describe("dropWhile method on List") {
    it("Should return empty list for dropWhile on empty list") {
      List.dropWhile(emptyList)(alwaysTrue) should equal (Nil)
    }
    it("Should return empty list if the predicate of each element is wrong")
  }
}
