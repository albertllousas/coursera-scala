package week2.infinitestreams

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Seq

/**
  * Created by aortiz on 8/02/17.
  */
class InfiniteStreamsTest extends FunSuite with Matchers {

  def infiniteStreamOfIntegers(): Stream[Int] = {
    def _from(n: Int): Stream[Int] = n #:: _from(n + 1)
    _from(0)
  }

  test("it should create an infinite stream for natural numbers") {

    // when
    val naturals: Seq[Int] = infiniteStreamOfIntegers()

    // then
    naturals.toString should be("Stream(0, ?)")
    naturals(3) should equal(3)
    naturals.toString should equal("Stream(0, 1, 2, 3, ?)")

  }

  test("it should create an infinite stream of all multiple of 4") {

    // when
    val multipleOfFour: Seq[Int] = infiniteStreamOfIntegers() map ( _ * 4 )

    // then
    multipleOfFour.toString should be("Stream(0, ?)")
    multipleOfFour(3) should equal(12)
    multipleOfFour.toString should equal("Stream(0, 4, 8, 12, ?)")

  }

  test("it should create an infinite stream of all prime numbers using sieve algorithm") {

    def from(n: Int): Stream[Int] = n #:: from(n+1)
    def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter (_ % s.head != 0))
    // when
    val primes: Seq[Int] = sieve(from(2))

    // then

    primes.toString should be("Stream(2, ?)")
    primes(3) should equal(7)
    primes.toString should equal("Stream(2, 3, 5, 7, ?)")

  }

}
