package coursera.fundesigninscala.week1.fortranslations

import coursera.fundesigninscala.week1.querieswithfor.{Book, BookDatabase}
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 6/02/17.
  */
class TranslationsOfForTest extends FunSuite with Matchers {

  test("simple for expression should be translated in terms of 'map'") {

    // given
    val f: Int => Int = x => x + 1
    val evaluateForExpression: Option[Int] => Option[Int] = m => for (x <- m) yield f(x)
    val evaluateMap: Option[Int] => Option[Int] = m => m map f

    // when
    val result = evaluateForExpression(Option(1))

    // then
    result should equal(evaluateMap(Option(1)))
    result shouldBe a[Some[Int]]
  }

  test("filtered for expression should be translated in terms of 'map' and 'withFilter'") {

    // given
    val f: Int => Int = x => x + 1
    val g: Int => Boolean = _ => true
    val evaluateForExpression: Option[Int] => Option[Int] = m => for {x <- m if g(x)} yield f(x)
    val evaluateFilterAndMap: Option[Int] => Option[Int] = m => m withFilter g map f

    // when
    val result = evaluateForExpression(Option(1))

    // then
    result should equal(evaluateFilterAndMap(Option(1)))
    result shouldBe a[Some[Int]]
  }

  test("nested for expression should be translated in terms of 'flatMap' and 'map'") {

    // given
    val f: Int => Int = x => x + 1
    val evaluateForExpression: (Option[Int], Option[Int]) => Option[Int] = (m1, m2) => for (x <- m1; y <- m2) yield f(x + y)
    val evaluateFlatMapAndMap: (Option[Int], Option[Int]) => Option[Int] = (m1, m2) => m1.flatMap(x => m2.map(y => f(x + y)))

    // when
    val result = evaluateForExpression(Option(1), Option(1))
    // then
    result should equal(evaluateFlatMapAndMap(Option(1), Option(1)))
    result shouldBe a[Some[Int]]
  }

  test("rich for expression should be translated in terms of 'flatMap', 'withFilter' and 'map'") {

    // given
    val books: List[Book] = BookDatabase.books

    // when
    val titlesFromFor = for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title
    val titlesFromFunctions = books.flatMap(b => b.authors withFilter (_.startsWith("Bird")) map (_ => b.title))

    // then
    titlesFromFor should equal (titlesFromFunctions)

  }

}
