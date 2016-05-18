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
    it("Should return None for empty list") {
      mean(emptyList) should equal (None)
    }
    it("Should return the mean for Double List") {
      mean(doubleList) should equal (Some(2.5))
    }
  }

  describe("variance") {
    it("Should return None for empty list") {
      variance(emptyList) should equal (None)
    }
    it("Should return the variance for Double List") {
      variance(doubleList) should equal (Some(1.25))
    }
  }
}
