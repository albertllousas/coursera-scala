package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 4/02/17.
  */
trait BasicGenerator[+T] {
  def generate: T
}

object BasicGenerator {

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
}