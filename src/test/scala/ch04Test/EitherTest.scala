package ch04Test

import org.scalatest.{FunSpec, Matchers}
import ch04._
/**
  * Created by fernandopratama on 20/5/16.
  */
class EitherTest extends FunSpec with Matchers {
  describe("map method on Either") {
    it("should return Left if the Either object is Left") {
      Left("Error").map((x: String) => x + "s") should equal (Left("Error"))
    }
    it("should return Right with its value applied by f if the Either object " +
      "is Right") {
      Right(12).map((x: Int) => 2*x) should equal (Right(24))
    }
  }

  describe("flatMap method on Either") {
    it("should return Left with function applied on the value on Left") {
      Left("Error").flatMap((x: String) => Left(x + "s")) should equal (Left
      ("Error"))
    }
    it("should return Right with function applied on the value on Right") {
      Right(12).flatMap((x: Int) => Right(x * 2)) should equal (Right(24))
    }
    it("should return Left with function applied on the value on Right") {
      Right(12).flatMap((x: Int) => Left("Error")) should equal (Left("Error"))
    }
  }

  describe("orElse method on Either") {
    it("should return the other value if the object is Left") {
      Left("Error").orElse(Right(12)) should equal (Right(12))
    }
    it("should return the object if the object is Right") {
      Right(12).orElse(Left("Error")) should equal (Right(12))
    }
  }

  describe("map2 method on Either") {
    it("should return the object itself if it is Left") {
      Left("Error").map2(Right(12))((x, y) => y) should equal (Left("Error"))
    }
    it("should return the other object if the other object is Left") {
      Right(12).map2(Left("Error"))((x, y) => x) should equal (Left("Error"))
    }
    it("should return the of f applied to content of the object and the other" +
      " if both are Right") {
      Right(12).map2(Right(17))(_ + _) should equal (Right(29))
    }
  }


}
