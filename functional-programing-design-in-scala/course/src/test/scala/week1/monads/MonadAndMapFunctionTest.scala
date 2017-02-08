package week1.monads

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by albert on 4/02/17.
  */
class MonadAndMapFunctionTest extends FunSuite with Matchers {

  test("'map' function should be defined for a monad as a => m map f == m flatMap (x => unit(f(x)))") {

    // given
    val unit: Int => Option[Int]  = x =>  Some(x)
    val monad = Some(3)
    val f: Int => Int = x => x+1

    // when
    val equality = () => (monad map f) == monad.flatMap(x => unit(f(x)))

    // then
    equality() shouldBe true

  }

}
