package ch03Test

import org.scalatest.{Matchers, FunSpec}
import ch03._

/**
  * Created by fernandopratama on 17/5/16.
  */
class ExCh03TreeTest extends FunSpec with Matchers {
  val leaf = Leaf(0)
  val intTree = Branch(Branch(Leaf(0), Leaf(1)), Leaf(2))
  val charTree = Branch(Branch(Leaf('a'), Leaf('b')),
    Branch(Leaf('c'), Leaf('d')))
  val doubleValue = (x: Int) => 2 * x
  val appendS = (x: Char) => x.toString + "s"

  describe("size on Tree method") {
    it("should return 1 for size on leaf") {
      Tree.size(leaf) should equal (1)
    }
    it("should return 5 for size on intTree") {
      Tree.size(intTree) should equal (5)
    }
    it("should return 7 for size on charTree") {
      Tree.size(charTree) should equal (7)
    }
  }

  describe("maximum on Tree method") {
    it("should return the value in the Leaf for maximum on leaf") {
      Tree.maximum(leaf) should equal (0)
    }
    it("should return the highest value in node for maximum on integer " +
      "branch") {
      Tree.maximum(intTree) should equal (2)
    }
  }

  describe("depth on Tree method") {
    it("should return 1 for depth on leaf") {
      Tree.depth(leaf) should equal (0)
    }
    it("should return 2 for Tree with one branch") {
      Tree.depth(intTree) should equal (2)
    }
    it("should return 2 for Tree with two branches, one branch on each side") {
      Tree.depth(charTree) should equal (2)
    }
  }

  describe("map on Tree method") {
    it("should apply the function on the value of leaf") {
      Tree.map(leaf)(_ + 1) should equal (Leaf(1))
    }
    it("should apply the function on each value on each leaf of integer " +
      "branch") {
      Tree.map(intTree)(doubleValue) should equal (Branch(Branch(Leaf(0),
        Leaf(2)), Leaf(4)))
    }
    it("should apply function on each value on each leaf of char branch") {
      Tree.map(charTree)(appendS) should
        equal (Branch(Branch(Leaf("as"), Leaf("bs")), Branch(Leaf("cs"), Leaf
        ("ds"))))
    }
  }
}
