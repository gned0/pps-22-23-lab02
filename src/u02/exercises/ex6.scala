package u02.exercises

import scala.annotation.tailrec

object ex6 extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (_, 0) => a
    case _ => gcd(b, a % b)

  println(gcd(8, 12))
  println(gcd(14, 7))

}
