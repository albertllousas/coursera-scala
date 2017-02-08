package week1.generator

import week1.generator.RandomTestHelper._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 6/02/17.
  */
class AdvancedGeneratorTest extends FunSuite with Matchers {

  // creating a integers implementation
  val integers = new AdvancedGenerator[Int] {
    val rand = new java.util.Random

    def generate: Int = rand.nextInt()
  }

  val booleans: AdvancedGenerator[Boolean] = integers map (_ > 0)

  test("generator should support for expressions to transform from random integers to booleans") {

    // given
    val booleans: AdvancedGenerator[Boolean] = for (x <- integers) yield x > 0

    // when
    val randomBooleans: Seq[Boolean] = executeNTimes(expr = booleans.generate)

    // then
    checkRandomnessOfSet(vals = randomBooleans, set = Set(true, false)) shouldBe true
  }

  test("generator should support 'map' to transform from random integers to booleans") {

    // given
    val booleans: AdvancedGenerator[Boolean] = integers map (_ > 0) // sugar for (x => x > 0)

    // when
    val randomBooleans: Seq[Boolean] = executeNTimes(expr = booleans.generate)

    // then
    checkRandomnessOfSet(vals = randomBooleans, set = Set(true, false)) shouldBe true
  }

  test("generator should support nested for expressions to create a pair generator") {

    // given
    val pairs: AdvancedGenerator[(Int, Int)] = for {
      i <- integers
      j <- integers
    } yield (i, j)


    // when
    val randomPairs: Seq[(Int, Int)] = executeNTimes(expr = pairs.generate)

    // then
    checkRandomness(vals = randomPairs) shouldBe true
  }

  test("generator should support 'flatMap' and 'map' to create a pair generator") {

    // given
    val pairs: AdvancedGenerator[(Int, Int)] =
      integers flatMap (x => integers map (y => (x, y)))

    // when
    val randomPairs: Seq[(Int, Int)] = executeNTimes(expr = pairs.generate)

    // then
    checkRandomness(vals = randomPairs) shouldBe true
  }

  test("generator should permit create the simplest generator") {

    // given
    def createSingleGenerator[T](x: T): AdvancedGenerator[T] = new AdvancedGenerator[T] {
      def generate: T = x
    }

    // when
    val single: AdvancedGenerator[Int] = createSingleGenerator(1)
    val result: Int = single.generate

    // then
    result should equal(1)
  }

  test("generator should permit create the ranged generator") {

    // given
    def choose(lo: Int, hi: Int): AdvancedGenerator[Int] =
      for (x <- integers) yield lo + Math.abs(x) % (hi - lo)

    // when
    val rangedGenerator: AdvancedGenerator[Int] = choose(0, 10000)
    val values: Seq[Int] = executeNTimes(times = 500, expr = rangedGenerator.generate)

    // then
    checkRandomness(vals = values, errorThresholdTolerance = 5) shouldBe true
    all(values) should be >= 0
    all(values) should be < 10000

  }

  test("generator should permit create a random element picker on sequences") {

    // given
    def choose(lo: Int, hi: Int): AdvancedGenerator[Int] =
      for (x <- integers) yield lo + Math.abs(x) % (hi - lo)

    def oneOf[T](xs: T*): AdvancedGenerator[T] =
      for (idx <- choose(0, xs.length)) yield xs(idx)

    // when
    val pickGenerator: AdvancedGenerator[String] = oneOf("one", "two", "three")
    val values: Seq[String] = executeNTimes(times = 500, expr = pickGenerator.generate)

    // then
    checkRandomnessOfSet(vals = values, set = Set("one", "two", "three")) shouldBe true


  }

  test("generator should permit create random lists") {

    // given
    def single[T](x: T): AdvancedGenerator[T] = new AdvancedGenerator[T] {
      def generate: T = x
    }

    def lists: AdvancedGenerator[List[Int]] = for {
      isEmpty <- booleans
      list <- if (isEmpty) emptyLists else nonEmptyLists
    } yield list

    def emptyLists = single(Nil)

//    def nonEmptyLists = integers flatMap( int => lists map(list => int :: list))
    def nonEmptyLists = for {
      head <- integers
      tail <- lists
    } yield head :: tail


    // when

    val values: Seq[Int] = lists.generate

    // then
    checkRandomness(vals = values) shouldBe true


  }

  test("generator should permit create random tree with for expressions") {
    // given
    def leafs: AdvancedGenerator[Leaf] = for {
      x <- integers
    } yield Leaf(x)

    def inners: AdvancedGenerator[Inner] = for {
      l <- trees
      r <- trees
    } yield Inner(l, r)

    def trees: AdvancedGenerator[Tree] = for {
      isLeaf <- booleans
      tree <- if (isLeaf) leafs else inners
    } yield tree

    // when
    val values: Seq[Tree] = executeNTimes(times = 50, expr = trees.generate)

    // then
    checkRandomness(vals = values) shouldBe true

  }

  test("generator should permit create random tree with using 'map' and 'flatMap' functions") {
    // given
    def leafs:  AdvancedGenerator[Leaf]  = integers map Leaf

    def inners: AdvancedGenerator[Inner] = trees.flatMap(left=> trees.map(right=> Inner(left,right)))

    def trees:AdvancedGenerator[Tree] = booleans flatMap(isLeaf => {
      if (isLeaf)
        leafs map( leaf => leaf)
      else
        inners map( inner => inner)
    })

    // when
    val values: Seq[Tree] = executeNTimes(times = 50, expr = trees.generate)

    // then
    checkRandomness(vals = values) shouldBe true

  }

}
