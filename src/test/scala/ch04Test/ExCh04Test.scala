package ch04Test

import org.scalatest.{Matchers, FunSpec}
import ch04.ExCh04._
import ch04.{None, Some}

/**
  * Created by fernandopratama on 18/5/16.
  */
class ExCh04Test extends FunSpec with Matchers {
  val emptyList: List[Double] = List[Double]()
  val doubleList = List[Double](1.0, 2.0, 3.0, 4.0)

  describe("mean") {
    it("should return None for empty list") {
      mean(emptyList) should equal (None)
    }
    it("should return the mean for Double List") {
      mean(doubleList) should equal (Some(2.5))
    }
  }

  describe("variance") {
    it("should return None for empty list") {
      variance(emptyList) should equal (None)
    }
    it("should return the variance for Double List") {
      variance(doubleList) should equal (Some(1.25))
    }
  }

  describe("bothMatch_2") {
    it("should return false if both patterns are wrong") {
      bothMatch_2("scala", "clojure", "python") should equal (Some(false))
    }
    it("should return false if one of the pattern is wrong") {
      bothMatch_2("scala", "clojure", "scala") should equal (Some(false))
    }
    it("should return true of both patterns are true") {
      bothMatch_2("(^hello)", "hello$", "hello") should
        equal (Some(true))
    }
  }

  describe("sequence") {
    it("should return Some(Nil) for empty list") {
      sequence(Nil) should equal (Some(Nil))
    }
    it("should return Some of list which contains the value of original " +
      "list") {
      sequence(List(Some(1), Some(2), Some(3))) should equal (Some(List(1, 2,
        3)))
    }
    it("should return None if the list contains None") {
      sequence(List(Some(1), None, Some(3))) should equal (None)
    }
  }
}
