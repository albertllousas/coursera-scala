package week2.structuralinduction

import org.scalatest.{FunSuite, Matchers}

/**
  * Given
  * P1 => Proposition 1 => Empty contains x       = false
  * P2 => Proposition 2 => (s incl x) contains x  = true
  * P3 => Proposition 3 => (xs incl y) contains x = xs contains x     if x != y
  */
class ProveCorrectnessOfImplOfIntSetByInductionTest extends FunSuite with Matchers {

  val p2Induction: (Int, Int, IntSet, IntSet) => Boolean = (x, y, l, r) => (NonEmpty(y, l, r) incl x) contains x
  val p3Induction: (Int, Int, Int, IntSet, IntSet) => Boolean =
    (z, x, y, l, r) => ((NonEmpty(z, l, r) incl y) contains x) == (NonEmpty(z, l, r) contains y)

  test("P1 should be proved") {

    // given
    val law: Int => Boolean = x => Empty contains x

    // then
    law(1) should equal(false)

  }


  test("P2 should be proved by Base case => (Empty incl x) contains x") {

    // given
    val law: Int => Boolean = x => (Empty incl x) contains x

    // then
    law(1) should equal(true)

  }

  test("P2 should be proved by Induction => (NonEmpty(x, l, r) incl x) contains x") {

    // then
    p2Induction(1, 1, Empty, Empty) should equal(true)
    p2Induction(1, 1, Empty incl 2, Empty incl 3) should equal(true)

  }

  test("P2 should be proved by Induction => (NonEmpty(y, l, r) incl x) contains x WHERE y < x") {


    // then
    p2Induction(4, 2, Empty, Empty) should equal(true)
    p2Induction(4, 2, Empty incl 6, Empty incl 1) should equal(true)

  }

  test("P2 should be proved by Induction => (NonEmpty(y, l, r) incl x) contains x WHERE y > x") {


    // then
    p2Induction(4, 5, Empty, Empty) should equal(true)
    p2Induction(4, 5, Empty incl 6, Empty incl 1) should equal(true)

  }

  test("P3 should be proved by Base case => (Empty incl y) contains x = Empty contains x") {

    // given
    val law: (Int, Int) => Boolean = (x, y) => ((Empty incl y) contains x) == (Empty contains x)

    // then
    law(4, 5) should equal(true)

  }


  test("P3 should be proved by Induction => (NonEmpty(z, l, r) incl y) contains x = NonEmpty(z, l, r) contains x WHERE z = x, z = y") {

    // then
    p3Induction(4, 4, 4, Empty, Empty) should equal(true)
    p3Induction(4, 4, 4, Empty incl 6, Empty incl 1) should equal(true)

  }

  test("P3 should be proved by Induction => (NonEmpty(z, l, r) incl y) contains x = NonEmpty(z, l, r) contains x WHERE z < y < x") {

    // then
    p3Induction(4, 5, 6, Empty, Empty) should equal(true)
    p3Induction(4, 5, 6, Empty incl 6, Empty incl 1) should equal(true)

  }

  test("P3 should be proved by Induction => (NonEmpty(z, l, r) incl y) contains x = NonEmpty(z, l, r) contains x WHERE y < z < x") {

    // then
    p3Induction(4, 3, 5, Empty, Empty) should equal(true)
    p3Induction(4, 3, 5, Empty incl 6, Empty incl 1) should equal(true)

  }

  test("P3 should be proved by Induction => (NonEmpty(z, l, r) incl y) contains x = NonEmpty(z, l, r) contains x WHERE y < x < z") {

    // then
    p3Induction(6, 4, 5, Empty, Empty) should equal(true)
    p3Induction(6, 4, 5, Empty incl 6, Empty incl 1) should equal(true)

  }

}
