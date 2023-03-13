package u02.exercises

object ex4 extends App {

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  println(p1(6)(8)(8))
  println(p2(6, 8, 8))
  println(p3(6)(8)(8))
  println(p4(6, 8, 8))
}
