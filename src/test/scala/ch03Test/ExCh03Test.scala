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
      List.drop(emptyList, 2) should equal (emptyList)
    }
    it("Should return the same list for dropping negative number of elements") {
      List.drop(intList, -10) should equal (intList)
    }
    it("Should behave like tail when dropping one element") {
      List.drop(intList, 1) should equal (List[Int](2, 3))
    }
    it("Should drop 2 elements if n = 2") {
      List.drop(charList, 2) should equal (List[Char]('c'))
    }
  }

  def alwaysTrue[A](x: A): Boolean = true
  def alwaysFalse[A](x: A): Boolean = false
  def isOdd(x: Int): Boolean = (x % 2) == 1

  describe("dropWhile method on List") {
    it("Should return empty list for dropWhile on empty list") {
      List.dropWhile(emptyList)(alwaysTrue) should equal (Nil)
    }
    it("Should return the same list if the predicate on each element is " +
      "wrong") {
      List.dropWhile(intList)(alwaysFalse) should equal (intList)
    }
    it("Should return the same list if the predicate on each element is true") {
      List.dropWhile(intList)(alwaysTrue) should equal (Nil)
    }
    it("Should stop taking element once the predicate is wrong") {
      List.dropWhile(intList)(isOdd) should equal (List[Int](2, 3))
    }
  }

  describe("setHead method on List") {
    it("Should return an empty list if setHead applied on empty list") {
      List.setHead(emptyList, 1) should equal (emptyList)
    }
    it("Should return list with replaced first element on integer list") {
      List.setHead(intList, 10) should equal (List[Int](10, 2, 3))
    }
    it("Should return list with replaced first element on char list") {
      List.setHead(charList, 'z') should equal (List[Char]('z', 'b', 'c'))
    }
  }
}
