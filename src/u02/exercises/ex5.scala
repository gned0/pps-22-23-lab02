package u02.exercises

object ex5 extends App {

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  println(compose(_ - 1, _ * 2)(5))

  def compose_generic[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  println(compose_generic[Int, Int, Int](_ - 1, _ * 2)(5))

}
