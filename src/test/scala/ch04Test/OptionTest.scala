package ch04Test

import org.scalatest.{Matchers, FunSpec}
import ch04._
/**
  * Created by fernandopratama on 17/5/16.
  */
class OptionTest extends FunSpec with Matchers {
  val optionInt = Some(1)
  val optionChar = Some('a')

  describe("map method on Option") {
    it("should return None for map on None") {
      None.map(x => x) should equal (None)
    }
    it("should return Some with value applied with function on Int Option") {
      optionInt.map(_ + 1) should equal (Some(2))
    }
    it("should return Some with value applied with function on Char Option") {
      optionChar.map(_.toString + "s") should equal(Some("as"))
    }
  }

  describe("getOrElse method on Option") {
    it("should return default for getOrElse on None") {
      None.getOrElse(1) should equal (1)
    }
    it("should return the value of Some on Int Option") {
      optionInt.getOrElse(10) should equal (1)
    }
    it("should return the value of Some on Char Option") {
      optionChar.getOrElse('b') should equal ('a')
    }
  }

  describe("flatMap method on Option") {
    it("should return None for flatMap on None") {
      None.flatMap(x => Some(x)) should equal (None)
    }
    it("should return Some with value applied with function for flatMap on " +
      "Int Option") {
      optionInt.flatMap((x: Int) => Some(x + 1)) should equal (Some(2))
    }
    it("should return Some with value applied with function for flatMap on " +
      "Char Option") {
      optionChar.flatMap((x: Char) => Some(x.toString + "s")) should
        equal (Some("as"))
    }
  }

  describe("orElse method on Option") {
    it("should return default for orElse on None") {
      None.orElse(Some(10)) should equal (Some(10))
    }
    it("should return the object for orElse on Some Int") {
      optionInt.orElse(Some(10)) should equal (Some(1))
    }
    it("should return the object for orElse on Some Char") {
      optionChar.orElse(Some('b')) should equal (Some('a'))
    }
  }

  describe("filter method on Option") {
    it("should return None for filter on None") {
      None.filter(x => true) should equal (None)
    }
    it("should return the object if the predicate on value is correct") {
      optionInt.filter(x => x%2 == 1) should equal (optionInt)
    }
    it("should return None if the predicate on value is wrong") {
      optionChar.filter(x => x%2 == 0) should equal (None)
    }
  }
}
