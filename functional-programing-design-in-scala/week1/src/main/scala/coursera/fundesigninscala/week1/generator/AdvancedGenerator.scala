package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 5/02/17.
  */
trait AdvancedGenerator[+T] {
  self =>
  // an alias for ”this”.
  def generate: T

  def map[S](f: T => S): AdvancedGenerator[S] = new AdvancedGenerator[S] {
    def generate: S = {
      val random = f(self.generate)
      random
    }
  }
  def flatMap[S](f: T => AdvancedGenerator[S]): AdvancedGenerator[S] = new AdvancedGenerator[S] {
    def generate: S = {
      val generator: AdvancedGenerator[S] = f(self.generate)
      val random: S = generator.generate
      random
    }
  }
}

