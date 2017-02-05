package coursera.fundesigninscala.week1.generator

/**
  * Created by albert on 5/02/17.
  */
trait AdvancedGenerator[+T] {
  self =>
  // an alias for ”this”.
  def generate: T
  def map[S](f: T => S): AdvancedGenerator[S] = new AdvancedGenerator[S] {
    def generate: S = f(self.generate)
  }
  def flatMap[S](f: T => AdvancedGenerator[S]): AdvancedGenerator[S] = new AdvancedGenerator[S] {
    def generate: S = f(self.generate).generate
  }
}
