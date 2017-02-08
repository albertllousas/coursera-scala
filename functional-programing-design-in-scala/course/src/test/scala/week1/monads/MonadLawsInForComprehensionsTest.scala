package week1.monads

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 3/02/17.
  */
class MonadLawsInForComprehensionsTest extends FunSuite with Matchers {

  test("a nested for expression should be written inline (Associativity)") {

    // given
    val monad = Some(3)
    val f: Int => Option[Int] = x => Some(x + 1)
    val g: Int => Option[Int] = x => Some(x + 2)
    val value = 1

    // when
    val nestedFor: Option[Int] = for {
      x <- monad
      y <- f(x)
      z <- g(y)
    } yield z
    val inlineFor: Option[Int] = for (y <- for (x <- monad; y <- f(x)) yield y; z <- g(y)) yield z

    // then
    nestedFor should equal(inlineFor)

  }

  test("right unit law should hold with for comprehensions => for (x <- m) yield x == m") {
    // given
    val monad = Some(3)
    val rightUnitInForExpression = (for (x <- monad) yield x) == monad
    // then
    rightUnitInForExpression shouldBe true
  }

}
