package week1.monads

import org.scalatest._


/**
  * Created by albert on 4/02/17.
  */

class MonadFunWithMyOwnImplOfTryTest extends FunSuite with Matchers {


  test("Try-valued computations should be composed in for expressions") {
    // when
    val resultOfComposedForComprehensions: Try[Int] = for {
      x <- Try(1 + 1)
      y <- Try(2 + 2)
    } yield x + y

    val resultOfFlatMapMapApplied = Try(1 + 1).flatMap(x => Try(2 + 2).map(y => x + y))

    // then
    resultOfComposedForComprehensions should equal(resultOfFlatMapMapApplied)

  }

  test("Try-valued crash computation in for expressions should be a failure") {
    // when
    val resultOfCrash: Try[Int] = for {
      x <- Try {
        1 / 0
      }
      y <- Try(2 + 2)
    } yield x + y

    // then
    resultOfCrash shouldBe a[Failure]
  }


  val unit: Int => Try[Int] = expr => Try(expr = expr)


  test("Try should hold 'Left Unit Law' => unit(x) flatMap f == f(x) ") {

    // given
    val f: Int => Try[Int] = x => Try(x + 1)
    val leftUnitLaw = (x: Int) => (unit(x) flatMap f) == f(x)
    val expr = 1 + 1
    // then
    leftUnitLaw(expr) shouldBe true

  }

  test("Try should violate 'Left Unit Law' => unit(x) flatMap f == f(x) when f(x) throws an exception") {

    // given
    val f: Int => Try[Int] = x => Try(x + 1)

    def leftSideOfUnitLaw(x: => Int) = {
      Try(x) flatMap f
    }

    def crash() = {
      1 / 0
    }

    // then
    an [ArithmeticException] should be thrownBy f(crash())
    leftSideOfUnitLaw(crash()) shouldBe a[Failure]

  }

  test("Try should hold 'Right Unit Law' => monad flatMap unit == monad ") {

    // given
    val monad = Try(3)
    val rightUnitLaw = (x: Int) => (monad flatMap unit) == monad
    val expr = 1 + 1
    // then
    rightUnitLaw(expr) shouldBe true

  }

  test("Try should hold 'Associative Law' => monad flatMap f flatMap g == monad flatMap (x => f(x) flatMap g) ") {

    // given
    val monad = Try(3)
    val f: Int => Try[Int] = x => Try(x + 1)
    val g: Int => Try[Int] = x => Try(x + 2)
    val associativeLaw = () => (monad flatMap f flatMap g) == (monad flatMap (x => f(x) flatMap g))

    // then
    associativeLaw() shouldBe true

  }

}
