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
  val doubleList = List[Double](1.0, 2.0, 3.0)
  def alwaysTrue[A](x: A): Boolean = true
  def alwaysFalse[A](x: A): Boolean = false
  def isOdd(x: Int): Boolean = (x % 2) == 1

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
      List.flatMap(List(charList, charList)) should equal (List[Char]('a',
        'b', 'c', 'a', 'b', 'c'))
    }
  }

  describe("add1 method on List") {
    it("should return empty list for add1 on empty list") {
      List.add1(Nil) should equal (Nil)
    }
    it("should return list of integer with each element incremented by 1") {
      List.add1(intList) should equal (List[Int](2, 3, 4))
    }
  }

  describe("doubleToString method in List") {
    it("should return empty list for doubleToString on empty list") {
      List.doubleToString(Nil) should equal (Nil)
    }
    it("should return list of String where each element is the same as " +
      "element in original list but in string type") {
      List.doubleToString(doubleList) should equal (List[String]("1.0", "2" +
        ".0", "3.0"))
    }
  }

  describe("map method in List") {
    it("should return empty list for map on empty list") {
      List.map(Nil)((x: Any) => x) should equal (Nil)
    }
    it("should return integer list with each element incremented by 1") {
      List.map(intList)(_ + 1) should equal (List[Int](2, 3, 4))
    }
    it("should return string list with each element appended by 's'") {
      List.map(charList)(_ + "s") should equal (List[String]("as", "bs", "cs"))
    }
  }

  describe("filter method in List") {
    it("should return empty list for filter on empty list") {
      List.filter(Nil)((x: Any) => true) should equal (Nil)
    }
    it("should return integer list only with elements which get true " +
      "predicate") {
      List.filter(intList)(isOdd) should equal (List[Int](1, 3))
    }
  }

  describe("flatMap1 method in List") {
    it("should return empty list for flatMap1 on empty list") {
      List.flatMap1(Nil)((x: Any) => List(x)) should equal (Nil)
    }
    it("should return integer list with each element incremented by 1") {
      List.flatMap1(intList)((x: Int) => List(x+1)) should equal (List(2, 3, 4))
    }
  }

  describe("filterViaflatMap1 method in List") {
    it("should return empty list for filter on empty list") {
      List.filterViaflatMap1(Nil)((x: Any) => true) should equal (Nil)
    }
    it("should return integer list only with elements which get true " +
      "predicate") {
      List.filterViaflatMap1(intList)(isOdd) should equal (List[Int](1, 3))
    }
  }

  describe("addList method in List") {
    it("should return empty list for adding empty list with integer list") {
      List.addList(Nil, intList) should equal (Nil)
      List.addList(intList, Nil) should equal (Nil)
    }
    it("should do element-wise addition given two integer lists") {
      List.addList(intList, intList) should equal (List[Int](2, 4, 6))
    }
  }

  describe("zipWith method in List") {
    it("should return empty list for zipping with empty list") {
      List.zipWith(intList, Nil)(_ + _) should equal(Nil)
    }
    it("should do element-wise addition with operator + on integer list") {
      List.zipWith(intList, intList)(_ + _) should equal(List[Int](2, 4, 6))
    }
    it("should do element-wise concatination with on char list") {
      List.zipWith(charList, charList)(_.toString + _.toString) should
        equal (List[String]("aa", "bb", "cc"))
    }
  }

  describe("hasSubsequence method in List") {
    it("should return true for an empty list to be subsequence of other " +
      "list") {
      List.hasSubsequence(intList, Nil: List[Int]) should equal(true)
    }
    it("should return false for a list with different order to be " +
      "subsequence of other list") {
      List.hasSubsequence(intList, List[Int](3, 2, 1)) should equal(false)
    }
    it("should return true for a list to be subsequence of the list itself") {
      List.hasSubsequence(intList, intList) should equal(true)
    }
    it("should return true for a list with fewer members but with correct " +
      "order to be subsequence of other list") {
      List.hasSubsequence(intList, List[Int](1, 2)) should equal(true)
    }
  }
}
