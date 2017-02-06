package coursera.fundesigninscala.week1.generator

import coursera.fundesigninscala.week1.generator.RandomTestHelper._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by albert on 5/02/17.
  */
class BasicGeneratorTest extends FunSuite with Matchers {

  val integers = new BasicGenerator[Int] {
    val rand = new scala.util.Random

    def generate: Int = rand.nextInt
  }

  val booleans = new BasicGenerator[Boolean] {
    def generate: Boolean = integers.generate > 0
  }

  val pairs = new BasicGenerator[(Int, Int)] {
    def generate: (Int, Int) = (integers.generate, integers.generate)
  }


  test("random generator should generate random integers") {

    // when
    val randomNumbers: Seq[Int] = executeNTimes(expr = integers.generate)

    // then
    checkRandomness(vals = randomNumbers) shouldBe true
  }

  test("random generator should generate random pairs") {

    // when
    val randomPairs: Seq[(Int, Int)] = executeNTimes(expr = pairs.generate)

    // then
    checkRandomness(vals = randomPairs) shouldBe true
  }

  test("random generator should generate random booleans") {

    // when
    val randomBooleans: Seq[Boolean] = executeNTimes(expr = booleans.generate)

    // then
    checkRandomnessOfSet(vals = randomBooleans, set = Set(true, false)) shouldBe true
  }


}
