package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 4/02/17.
  */
trait BasicGenerator[+T] {
  def generate: T
}
