package coursera.fundesigninscala.week1.generator

import coursera.fundesigninscala.week1.generator.RandomTestHelper._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by albert on 5/02/17.
  */
class BasicGeneratorTest extends FunSuite with Matchers {



  def executeNTimes[T](times:Int=100,expr: => T): Seq[T] = (1 to times).map(_ => expr)

  test("random generator should generate random integers") {

    // when
    val randomNumbers: Seq[Int] = executeNTimes(expr=BasicGenerator.integers.generate)

    // then
    checkRandomness(vals=randomNumbers) shouldBe true
  }

  test("random generator should generate random pairs") {

    // when
    val randomPairs: Seq[(Int,Int)] = executeNTimes(expr=BasicGenerator.pairs.generate)

    // then
    checkRandomness(vals=randomPairs) shouldBe true
  }

  test("random generator should generate random booleans") {

    // when
    val randomBooleans: Seq[Boolean] = executeNTimes(expr=BasicGenerator.booleans.generate)

    // then
    checkRandomnessOfBooleans(vals=randomBooleans) shouldBe true
  }

  test("generator does not support for expressions") {

    // then
    "val booleans = executeNTimes(expr=BasicGenerator.booleans.generate)" should compile
    "val booleans = for (x <- BasicGenerator.integers.generate) yield x > 0" shouldNot compile
  }

}
