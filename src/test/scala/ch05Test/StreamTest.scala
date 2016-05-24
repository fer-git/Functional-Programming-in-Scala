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

  describe("forAll method in Stream") {
    it("should return true for Empty Stream") {
      Empty.forAll(x => false) should equal (true)
    }
    it("should return false if the predicate of at least one the element is " +
      "false") {
      Stream(1, 2, 3).forAll(x => x%2 == 1) should equal (false)
    }
    it("should return true if the predicate of all elements are true") {
      Stream(2, 4, 6).forAll(x => x%2 == 0) should equal (true)
    }
  }

  describe("takeWhileViaFoldRight method in Stream") {
    it("should return Empty for Empty Stream") {
      Empty.takeWhileViaFoldRight(x => true) should equal (Empty)
    }
    it("should return all starting elements which predicate return true") {
      Stream(1, 3, 5, 4, 7).takeWhileViaFoldRight(x => x%2 == 1).toList should
        equal (Stream(1, 3, 5).toList)
    }
  }

  describe("map method in Stream") {
    it("should return Empty for Empty Stream") {
      Empty.map(x => true) should equal (Empty)
    }
    it("should return Cons with each element applied by function on Cons") {
      Stream(1, 2, 3).map((x: Int) => x + 1).toList should
        equal(Stream(2, 3, 4).toList)
    }
  }

  describe("filter method in Stream") {
    it("should return Empty for Empty Stream") {
      Empty.filter(x => true) should equal (Empty)
    }
    it("should return Empty if predicate on each element is false") {
      Stream(2, 4, 6).filter(_%2 == 1) should equal (Empty)
    }
    it("should return Stream only if the predicate on element is true") {
      Stream(1, 2, 3).filter(_%2 == 1).toList should equal (Stream(1, 3).toList)
    }
  }

  describe("append method in Stream") {
    it("should return original Stream if appended by Empty") {
      Stream(1, 2, 3).append(Empty).toList should equal (Stream(1, 2, 3).toList)
    }
    it("should append two Streams into one Stream") {
      Stream(1, 2, 3).append(Stream(4, 5)).toList should
      equal (Stream(1, 2, 3, 4, 5).toList)
    }
  }

  describe("flatMap method in Stream") {
    it("should return Empty for Empty Stream") {
      Empty.flatMap(x => Stream(1)) should equal (Empty)
    }
    it("should return Stream with each element applied by function and " +
      "flatted") {
      Stream(1, 2, 3).flatMap(x => Stream(x + 1)).toList should equal (Stream
        (2, 3, 4).toList)
    }
  }

  describe("constant method in Stream") {
    it("should return Stream with the same elements") {
      Stream.constant(10).take(3).toList should equal (Stream(10, 10, 10)
        .toList)
    }
  }

  describe("from method in Stream") {
    it("should return Stream with increasing sequence starting from -1") {
      Stream.from(-1).take(3).toList should equal(Stream(-1, 0, 1).toList)
    }
  }

  describe("fibs in Stream") {
    it("should return Stream with one element from fibonacci series") {
      Stream.fibs.take(1).toList should equal (List(0))
    }
    it("should return Stream with 3 elements from fibonacci series") {
      Stream.fibs.take(3).toList should equal (List(0, 1, 1))
    }
  }

  describe("unfold in Stream") {
    it("should return Stream with increasing sequence starts 1") {
      Stream.unfold(1)(x => Some((x, x+1))).take(3).toList should
        equal (Stream(1, 2, 3).toList)
    }
    it("should return Stream with increasing number of s") {
      Stream.unfold("x")(x => Some((x, x + "s"))).take(3).toList should
        equal (Stream("x", "xs", "xss").toList)
    }
  }

  describe("fibsViaUnfold in Stream") {
    it("should return Stream with one element from fibonacci series") {
      Stream.fibsViaUnfold.take(1).toList should equal (List(0))
    }
    it("should return Stream with 3 elements from fibonacci series") {
      Stream.fibsViaUnfold.take(3).toList should equal (List(0, 1, 1))
    }
  }

  describe("fromViaUnfold in Stream") {
    it("should return Stream with increasing sequence starting from -1") {
      Stream.fromViaUnfold(-1).take(3).toList should equal (List(-1, 0, 1))
    }
  }

  describe("constantViaUnfold in Stream") {
    it("should return Stream with the same elements") {
      Stream.constantViaUnfold(10).take(3).toList should equal (List(10, 10 ,
        10))
    }
  }

  describe("onesViaUnfold in Stream") {
    it("should return Stream with elements 1") {
      Stream.onesViaUnfold.take(3).toList should equal (List(1, 1, 1))
    }
  }

  describe("mapViaUnfold method in Stream") {
    it("should return Empty for an Empty Stream") {
      Empty.mapViaUnfold(x => 1) should equal (Empty)
    }
    it("should return Cons with each element applied by function on Cons") {
      Stream(1, 2, 3).mapViaUnfold((x: Int) => x + 1).toList should
        equal(Stream(2, 3, 4).toList)
    }
  }

  describe("takeViaUnfold method in Stream") {
    it("should return empty for Empty Stream") {
      Empty.takeViaUnfold(10) should equal (Empty)
    }
    it("should return Stream with the first two elements from original " +
      "Stream") {
      Stream(1, 2, 3).takeViaUnfold(2).toList should equal (Stream(1, 2).toList)
    }
  }

  describe("takeWhileViaUnfold method in Stream"){
    it("should return empty for Empty Stream") {
      Empty.takeWhileViaUnfold(x => true) should equal (Empty)
    }
    it("should return all starting elements which predicate return true") {
      Stream(1, 3, 5, 4, 7).takeWhileViaUnfold(x => x%2 == 1).toList should
        equal (Stream(1, 3, 5).toList)
    }
  }

  describe("zip method in Stream") {
    it("should return Empty for Empty Stream") {
      Empty.zip(Stream(1, 2, 3)) should equal (Empty)
    }
    it("should return Stream with elements paired with other Stream") {
      Stream(1, 2, 3).zip(Stream('a', 'b', 'c')).toList should
      equal (List((1, 'a'), (2, 'b'), (3, 'c')))
    }
    it("should return Stream with length equal to the shortest input Stream") {
      Stream(1, 2).zip(Stream('a', 'b', 'c')).toList should
      equal (List((1, 'a'), (2, 'b')))
    }
  }

  describe("zipAll method in Stream") {
    it("should return Empty if both are empty") {
      Empty.zipAll(Empty) should equal (Empty)
    }
    it("should return Stream with elements pair with other Stream") {
      Stream(1, 2, 3).zipAll(Stream('a', 'b', 'c')).toList should
      equal (List((Some(1), Some('a')), (Some(2), Some('b')), (Some(3), Some
      ('c'))))
    }

    it("should return Stream with length equal to the longest input Stream") {
      Stream(1, 2).zipAll(Stream('a', 'b', 'c')).toList should
      equal (List((Some(1), Some('a')), (Some(2), Some('b')), (None, Some('c'))))
    }
  }

}
