package week2.streams

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Seq

/**
  * Created by aortiz on 7/02/17.
  */


class FunWithStreamsTest extends FunSuite with Matchers {

  // TODO: this class could be totally improved, but for now works
  class SeqAccessRegistry {
    var streamAccessCounter = 0
    var listAccessCounter = 0

    def streamRange(lo: Int, hi: Int): Stream[Int] = {
      def _streamRange(lo: Int, hi: Int): Stream[Int] =
        if (lo >= hi) Stream.empty
        else {
          streamAccessCounter = streamAccessCounter + 1
          lo #:: _streamRange(lo + 1, hi)
        }

      streamAccessCounter = 0
      _streamRange(lo, hi)
    }

    def listRange(lo: Int, hi: Int): List[Int] = {

      def _listRange(lo: Int, hi: Int): List[Int] =
        if (lo >= hi) Nil
        else {
          listAccessCounter = listAccessCounter + 1
          lo :: _listRange(lo + 1, hi)
        }

      listAccessCounter = 0
      _listRange(lo, hi)
    }

  }

  test("stream creation recursive method should be called only one time if resulting stream is not accessed") {

    // given
    val registry = new SeqAccessRegistry

    // when
    val stream: Seq[Int] = registry.streamRange(0, 10)
    val list: Seq[Int] = registry.listRange(0, 10)

    // then
    registry.streamAccessCounter should equal(1)
    registry.listAccessCounter should equal(10)


  }

  test("stream creation recursive method should be called 'n+1' times when resulting stream is accessed at index n") {

    // given
    val registry = new SeqAccessRegistry

    // when
    val stream: Stream[Int] = registry.streamRange(0, 10)
    val list: Seq[Int] = registry.listRange(0, 10)
    stream(3)
    stream(1)
    stream(2)
    stream(2)
    stream(2)

    // then
    registry.streamAccessCounter should equal(4)
    registry.listAccessCounter should equal(10)

  }

}
