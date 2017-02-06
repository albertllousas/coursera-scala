package coursera.fundesigninscala.week1.querieswithfor

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by aortiz on 6/02/17.
  */
class QueriesWithForTest extends FunSuite with Matchers {

  test("It should be able to find titles of books whose author’s name is “Bird” using for expressions as queries") {

    // when
    val titles: List[String] = for (b <- BookDatabase.books; a <- b.authors if a startsWith "Bird,") yield b.title

    // then
    titles should have size 1
    titles.head should equal ("Introduction to Functional Programming")
  }

  test("It should be able to find titles of books which have the word “Program” in the title") {

    // when
    val titles: List[String] = for (b <- BookDatabase.books; if b.title.indexOf("Program") >= 0) yield b.title

    // then
    titles should have size 3
  }

  test("It should be able to find the names of all authors who have written at least two books present in the database") {

    // when
    val authors: List[String] = for {
      b1 <- BookDatabase.books
      b2 <- BookDatabase.books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1

    // then
    authors should have size 1
    authors.head should equal ("Bloch, Joshua")

  }

  test("It should be able to find the names of all authors who have written at least two books present in the database without repeats") {

    // given
    val books: List[Book] = List(
      Book(
        title = "Structure and Interpretation of Computer Programs",
        authors = List("Bloch, Joshua")
      ),
      Book(
        title = "Introduction to Functional Programming",
        authors = List("Bloch, Joshua")
      ),
      Book(
        title = "Effective Java",
        authors = List("Bloch, Joshua")
      )
    )
    val bookSet = books.toSet
    // when
    val authors= for {
      b1 <- bookSet
      b2 <- bookSet
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1

    // then
    authors should have size 1
    authors.head should equal ("Bloch, Joshua")

  }

}
