package week2.structuralinduction

import org.scalatest.{FunSuite, Matchers}

class IntSetTest extends FunSuite with Matchers {

  test("set should be generated with internal structure as a binary tree") {

    // given
    val expectedTree = NonEmpty(4, NonEmpty(2, Empty, NonEmpty(3, Empty, Empty)), NonEmpty(5, Empty, Empty))

    // when
    val set: IntSet = Empty incl 4 incl 2 incl 3 incl 5

    // then
    set shouldBe a[NonEmpty]
    set.asInstanceOf[NonEmpty] should equal(expectedTree)

  }

  test("set should not have repeated elements internally when repeated elements are included") {

    // given
    val expectedTree = NonEmpty(4, Empty, Empty)

    // when
    val set: IntSet = Empty incl 4 incl 4 incl 4

    // then
    set shouldBe a[NonEmpty]
    set.asInstanceOf[NonEmpty] should equal(expectedTree)

  }

  test("contains function of set should work either on exiting elements or non existing elements") {

    // given
    val set: IntSet = Empty incl 4 incl 2 incl 3 incl 5

    // when
    val isFiveIncluded = set contains 5
    val isTenIncluded = set contains 10

    // then
    isFiveIncluded should equal(true)
    isTenIncluded should equal(false)

  }

}
