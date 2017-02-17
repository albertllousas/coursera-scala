package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

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

  property("insert two elements into an empty heap, then find the minimum, should return a minimum of the two input elements") =
    forAll { (n1: A, n2: A) =>
      val h = insert(n1, insert(n2, empty))
      val smallest = if (n1 < n2) n1 else n2
      findMin(h) == smallest
    }

  property("insert two elements into an empty heap, then delete the minimum and then find the minimum, should return a maximum of the two input elements") =
    forAll { (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      findMin(deleteMin(h)) == scala.math.max(a, b)
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (heap1: H, heap2: H) =>
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)
      val meldedHeap = meld(heap1, heap2)
      val minOfMeld = findMin(meldedHeap)
      minOfMeld == min1 || minOfMeld == min2
    }

  property("Given two heaps, melding each other in any order, the minimum should be the same") =
    forAll { (h1: H, h2: H) =>
      val m1 = meld(h1, h2)
      val m2 = meld(h2, h1)

      val minMeld1 = findMin(m1)
      val minMeld2 = findMin(m2)

      minMeld1 == minMeld2
    }

  //(Hint: recursion and helper functions are your friends.)
  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minim.") =
    forAll { h: H =>
      def _toOrderedListFoldingRight(currentHeap: H, accumulated: List[Int]): List[Int] = currentHeap match {
        case heap if isEmpty(heap) => accumulated
        case heap => findMin(heap) :: _toOrderedListFoldingRight(deleteMin(heap), accumulated)
      }

      @tailrec
      def _toOrderedListFoldingLeft(currentHeap: H, accumulated: List[Int]): List[Int] = currentHeap match {
        case heap if isEmpty(heap) => accumulated
        case heap => _toOrderedListFoldingLeft(deleteMin(heap), accumulated ::: List(findMin(heap)))
      }

      val xs = _toOrderedListFoldingLeft(h, List.empty)
      xs == xs.sorted
    }

  property("Given two heaps, after find the minimum from one heap, remove it, insert in the other heap and finally meld two heaps, " +
    "should return the minimal of the previous ones") =
    forAll { (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val smallest = if (m1 < m2) m1 else m2
      val meldedHeap = meld(deleteMin(h1), insert(smallest, h2))
      findMin(meldedHeap) == smallest
    }

  property("Given a list of random integers, after sort the list and insert in an empty list, should return the minimums, " +
    "when find and delete recursively") =
    forAll { list: List[Int] =>
      def _recursiveCheckMins(h: H, l: List[Int]): Boolean = {
        if (isEmpty(h)) {
          l.isEmpty
        } else {
          l.nonEmpty && findMin(h) == l.head && _recursiveCheckMins(deleteMin(h), l.tail)
        }
      }

      val sortedList = list.sorted
      val heap = sortedList.foldLeft(empty)((heap, element) => insert(element, heap))
      _recursiveCheckMins(heap, sortedList)
    }



}
