package week1.monads

import org.scalatest._

/**
  * Created by aortiz on 3/02/17.
  */
class MonadLawsWithOptionalTest extends FunSuite with Matchers {

  val unit: Int => Option[Int]  = x =>  Some(x)


  test("an Optional monad should hold 'Left Unit Law' => unit(x) flatMap f == f(x) ") {

    // given
    val f: Int => Option[Int] = x => Some(x+1)
    val leftUnitLaw = (x:Int) => (unit(x) flatMap f) == f(x)
    val value = 1
    // then
    leftUnitLaw(value) shouldBe true

  }

  test("an Optional monad should hold 'Right Unit Law' => monad flatMap unit == monad ") {

    // given
    val monad = Some(3)
    val rightUnitLaw = (x:Int) => (monad flatMap unit) == monad
    val value = 1
    // then
    rightUnitLaw(value) shouldBe true

  }

  test("an Optional monad should hold 'Associative Law' => monad flatMap f flatMap g == monad flatMap (x => f(x) flatMap g) ") {

    // given
    val monad = Some(3)
    val f: Int => Option[Int] = x => Some(x+1)
    val g: Int => Option[Int] = x => Some(x+2)
    val associativeLaw = () => (monad flatMap f flatMap g) == (monad flatMap (x => f(x) flatMap g))

    // then
    associativeLaw() shouldBe true

  }
}