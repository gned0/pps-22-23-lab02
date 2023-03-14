package u02.exercises

import u02.ProductTypes.Point2D
import math._
import scala.annotation.tailrec

object exercises extends App{

  // exercise 3a
  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  val positiveLambda: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println("EXERCISE 3A, expected output: (p, p, n, n)")
  println()
  println(positiveMethod(3))
  println(positiveMethod(-3))

  println(positiveLambda(3))
  println(positiveLambda(-3))
  println()
  // exercise 3b

  val negLambda: (String => Boolean) => String => Boolean =
    f => s => !f(s)

  def negMethod(f: String => Boolean): String => Boolean =
    (s: String) => !f(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = negLambda(empty)

  println("EXERCISE 3B, expected output: true")
  println()
  println(notEmpty("foo") && !notEmpty(""))
  println()

  // exercise 3c

  def negMethodGeneric[A](f: A => Boolean): A => Boolean =
    (s: A) => !f(s)

  // exercise 4

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  println("EXERCISE 4, expected output: true x4")
  println()
  println(p1(6)(8)(8))
  println(p2(6, 8, 8))
  println(p3(6)(8)(8))
  println(p4(6, 8, 8))
  println()

  // exercise 5

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  println("EXERCISE 5, expected output: 9")
  println()
  println(compose(_ - 1, _ * 2)(5))
  println()

  // exercise 6

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

  println("EXERCISE 6, expected output: 4, 7")
  println()
  println(gcd(8, 12))
  println(gcd(14, 7))

  // exercise 7

  enum Shape:
    case Triangle(a: Point2D, b: Point2D, c: Point2D)
    case Circle(center: Point2D, radius: Double)
    case Square(origin: Point2D, side: Double)

  object Shape:
    def distance(a: Point2D, b: Point2D): Double =
      sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2))
    def perimeter(s: Shape): Double = s match
      case Triangle(a, b, c) => distance(a, b) + distance(a, c) + distance(b, c)
      case Circle(c, r) => 2 * 3.14 * r
      case Square(o, s) => 4 * s
    def contains(s:Shape, p: Point2D): Boolean = s match
      case Triangle(a, b, c) => true
      case Circle(c, r) => pow(p.x - c.x, 2) + pow(p.x - c.y, 2) < pow(r, 2)
      case Square(o, s) => (p.x >= o.x && p.x <= o.x + s) && (p.y >= o.y && p.y <= o.y + s)


  import Shape.*
  println("EXERCISE 7")
  println()
  println(perimeter(Triangle(Point2D(0, 0), Point2D(1, 1), Point2D(2, 2))))
  println(perimeter(Circle(Point2D(0, 0), 2)))
  println(perimeter(Square(Point2D(0, 0), 4)))
  println(contains(Circle(Point2D(0, 0), 2), Point2D(4, 4)))
  println(contains(Square(Point2D(0, 0), 2), Point2D(1, 1)))
}
