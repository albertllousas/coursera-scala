package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    randomInt <- arbitrary[Int]
    randomHeap <- oneOf(const(empty), genHeap)
  } yield insert(randomInt, randomHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back") =
    forAll { (int1: Int, int2: Int) =>
      val heap = insert(int2, insert(int1, empty))
      val smallest = if (int1 < int2) int1 else int2
      findMin(heap) == smallest
    }

  property("insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { int: Int =>
      val heap = insert(int, empty)
      val result = deleteMin(heap)
      result == empty
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (heap1: H, heap2: H) =>
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)
      val meldedHeap = meld(heap1, heap2)
      val minOfMeld = findMin(meldedHeap)
      minOfMeld == min1 || minOfMeld == min2
    }

  //(Hint: recursion and helper functions are your friends.)
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minim.") =
    forAll { h: H =>
      def _toOrderedListFoldingRight(currentHeap: H, accumulated: List[Int]): List[Int] = currentHeap match {
        case heap if isEmpty(heap) => accumulated
        case heap => findMin(heap) :: _toOrderedListFoldingRight(deleteMin(heap), accumulated)
      }
      val xs = _toOrderedListFoldingRight(h, Nil)
      xs == xs.sorted
    }
}
