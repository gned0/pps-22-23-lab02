package u02.exercises

import scala.annotation.tailrec

object ex6 extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (a, b) if a == 0 => b
    case (a, b) if b == 0 => a
    case (a, b) if a > b => (a, b) match
      case (a, b) if a % b == 0 => b
      case (a, b) => gcd(b, a % b)
    case (a, b) if a < b => (a, b) match
      case (a, b) if b % a == 0 => a
      case (a, b) => gcd(a, b % a)

  println(gcd(8, 12))
  println(gcd(14, 7))

}
