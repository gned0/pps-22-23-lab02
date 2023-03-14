package u02.exercises

object ex5 extends App {

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  println(compose(_ - 1, _ * 2)(5))
  
}
