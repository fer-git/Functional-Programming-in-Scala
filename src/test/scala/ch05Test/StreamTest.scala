package ch05Test

import org.scalatest.{Matchers, FunSpec}
import ch05._

/**
  * Created by fernandopratama on 21/5/16.
  */
class StreamTest extends FunSpec with Matchers {
  describe("toList method in Stream") {
    it("should return Nil for Empty Stream") {
      Empty.toList should equal (Nil)
    }
    it("should return list for Cons Stream") {
      Stream(1, 2, 3).toList should equal (List(1, 2, 3))
    }
  }

  describe("take method in Stream") {
    it("should return empty for Empty Stream") {
      Empty.take(10) should equal (Empty)
    }
    it("should return Stream with the first two elements from original " +
      "Stream") {
      Stream(1, 2, 3).take(2).toList should equal (Stream(1, 2).toList)
    }
  }

  describe("takeWhile method in Stream") {
    it("should return empty for Empty Stream") {
      Empty.takeWhile(x => true) should equal (Empty)
    }
    it("should return all starting elements which predicate return true") {
      Stream(1, 3, 5, 4, 7).takeWhile(x => x%2 == 1).toList should
        equal (Stream(1, 3, 5).toList)
    }
  }
}
