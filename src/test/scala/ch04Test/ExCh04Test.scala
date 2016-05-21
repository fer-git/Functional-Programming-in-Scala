package ch04Test

import org.scalatest.{Matchers, FunSpec}
import ch04.ExCh04._
import ch04.{None, Some}
import ch04.{Left, Right}

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

  describe("traverse") {
    it("should return Some(Nil) for empty list") {
      traverse(Nil)((x: Int) => Some(x)) should equal (Some(Nil))
    }
    it("should return None if at least one of the element produced None by f") {
      traverse(List(1, 2))((x: Int) => None) should equal (None)
    }
    it("should return Some of List if none of the element produced None by f") {
      traverse(List(1, 2))((x: Int) => Some(x + 1)) should equal (Some(List
      (2, 3)))
    }
  }

  describe("traverseEither") {
    it("should return Right Nil for empty list") {
      traverseEither(Nil)(x => Left(x)) should equal (Right(Nil))
    }
    it("should return Left if at least one of the element returns Left") {
      traverseEither(List(10, 1, 2))(x => Left(x)) should
        equal (Left(10))
    }
    it("should return Right if none of the element returns Left") {
      traverseEither(List(1, 2, 3))((x: Int) => Right(x+1)) should
        equal (Right(List(2, 3, 4)))
    }
  }

  describe("sequenceEither") {
    it ("should return Right Nil for empty list") {
      sequenceEither(Nil) should equal (Right(Nil))
    }
    it("should return Left if at least of the element is Left") {
      sequenceEither(List(Right(10), Left("Error"), Right(1))) should
      equal (Left("Error"))
    }
    it("should return Right if all elements are Right") {
      sequenceEither(List(Right(1), Right(2), Right(3))) should
      equal (Right(List(1, 2, 3)))
    }
  }
}
