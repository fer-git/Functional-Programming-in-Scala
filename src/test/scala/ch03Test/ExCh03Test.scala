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

  describe("init method on List") {
    it("Should return empty list on init empty list") {
      List.init(emptyList) should equal (Nil)
    }
    it("Should return empty list on init single element list") {
      List.init(List[Int](1)) should equal (Nil)
    }
    it("Should return original list except the last element on integer list") {
      List.init(intList) should equal (List[Int](1, 2))
    }
    it("Should return original list except the last element on char list") {
      List.init(charList) should equal (List[Char]('a', 'b'))
    }
  }

  describe("length method on List") {
    it("Should return 0 for an empty list") {
      List.length(emptyList) should equal (0)
    }
    it("Should return 3 for integer list with 3 elements") {
      List.length(intList) should equal (3)
    }
    it("Should return 3 for char list with 3 elements") {
      List.length(charList) should equal (3)
    }
  }

  describe("foldLeft method on List") {
    it("Should return accumulator for an empty list") {
      List.foldLeft(emptyList, 0)((x: Int, y: Int) => x + y) should equal (0)
    }
    it("Should return sum of each element in integer list") {
      List.foldLeft(intList, 0)((x: Int, y: Int) => x + y) should equal (6)
    }
    it("Should return string of concatenated char in char list") {
      List.foldLeft(charList, "")((x: String, y: Char) => x + y) should
        equal ("abc")
    }
  }

  val doubleList = List[Double](1.0, 2.0, 3.0)
  describe("foldLeft version of sum, product, and length on List") {
    it("sum3 should return the same result as its foldRight version, sum2") {
      List.sum3(intList) should equal (List.sum2(intList))
    }
    it("product3 should return the same result as its foldRight version, " +
      "product2") {
      List.product3(doubleList) should equal (List.product2(doubleList))
    }
    it("length2 should return the same result as its foldRight version, " +
      "length") {
      List.length2(charList) should equal (List.length(charList))
    }
  }

  describe("reverse method on List") {
    it("Should return empty list for reverse on empty list") {
      List.reverse(emptyList) should equal (Nil)
    }
    it("Should return list with reversed element for reverse on integer list") {
      List.reverse(intList) should equal (List[Int](3, 2, 1))
    }
    it("Should return list with reversed element for reverse on char list") {
      List.reverse(charList) should equal (List[Char]('c', 'b', 'a'))
    }
  }

  describe("foldLeft2 method on List") {
    it("should return the same result as foldLeft on empty list") {
      List.foldLeft2(emptyList, 0)((x: Int, y: Int) => x + y) should
        equal (List.foldLeft(emptyList, 0)((x: Int, y: Int) => x + y))
    }
    it("should return the same result as foldLeft on integer list") {
      List.foldLeft2(intList, 0)((x: Int, y: Int) => x + y) should
        equal (List.foldLeft(intList, 0)((x: Int, y: Int) => x + y))
    }
    it("should return the same result as foldRight on char list") {
      List.foldLeft2(charList, "")((x: String, y: Char) => x + y) should
        equal (List.foldLeft(charList, "")((x: String, y: Char) => x + y))
    }
  }

  describe("appendFL method on List") {
    it("should return the same list if it is appended by empty list") {
      List.appendFL(intList, emptyList) should equal (intList)
    }
    it("should return a list with combined elements on integer list") {
      List.appendFL(intList, intList) should equal(List[Int](1, 2, 3, 1, 2, 3))
    }
    it("should return a list with combined elements on char list") {
      List.appendFL(charList, charList) should equal (List[Char]('a', 'b',
        'c', 'a', 'b', 'c'))
    }
  }

  describe("appendFR method on List") {
    it("should return the same list if it is appended by empty list") {
      List.appendFR(intList, emptyList) should equal(intList)
    }
    it("should return a list with combined elements on integer list") {
      List.appendFR(intList, intList) should equal(List[Int](1, 2, 3, 1, 2, 3))
    }
    it("should return a list with combined elements on char list") {
      List.appendFR(charList, charList) should equal(List[Char]('a', 'b',
        'c', 'a', 'b', 'c'))
    }
  }

  describe("flatMap method on List") {
    it("should return empty list for flatMap on list that contains empty " +
      "list") {
      List.flatMap(List(emptyList)) should equal (Nil)
    }
    it("should concatenated a list of two integer lists into an integer list") {
      List.flatMap(List(intList, intList)) should equal (List[Int](1, 2, 3,
        1, 2, 3))
    }
    it("should concatenated a list of two char lists into a char list") {
      List.flatMap((List(charList, charList))) should equal (List[Char]('a',
        'b', 'c', 'a', 'b', 'c'))
    }
  }
}
