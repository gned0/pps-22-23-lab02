package u02.exercises

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

  println("EXERCISE 3A")
  println("Expected: positive, actual: " + positiveMethod(3))
  println("Expected: negative, actual: " + positiveMethod(-3))
  println("Expected: positive, actual: " + positiveLambda(3))
  println("Expected: negative, actual: " + positiveLambda(-3))
  println()

  // exercise 3b
  val negLambda: (String => Boolean) => String => Boolean =
    f => s => !f(s)

  def negMethod(f: String => Boolean): String => Boolean =
    (s: String) => !f(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = negLambda(empty)

  println("EXERCISE 3B")
  println("Expected: true, actual: " + (notEmpty("foo") && !notEmpty("")))
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

  println("EXERCISE 4")
  println("Expected: true, actual: " + p1(6)(8)(8))
  println("Expected: true, actual: " + p2(6, 8, 8))
  println("Expected: true, actual: " + p3(6)(8)(8))
  println("Expected: true, actual: " + p4(6, 8, 8))
  println()

  // exercise 5
  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  println("EXERCISE 5")
  println("Expected: 9, actual: " + compose(_ - 1, _ * 2)(5))
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

  println("EXERCISE 6")
  println("Expected output: 4, actual: " + gcd(8, 12))
  println("Expected output: 7, actual: " + gcd(14, 7))
  println()

  // exercise 7
  case class Point2D(x: Double, y: Double)
  enum Shape:
    case Triangle(a: Point2D, b: Point2D, c: Point2D)
    case Circle(center: Point2D, radius: Double)
    case Square(origin: Point2D, side: Double)

  object Shape:
    def distance(a: Point2D, b: Point2D): Double =
      sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2))
    def pointInTriangle(p: Point2D, a: Point2D, b: Point2D, c: Point2D): Boolean =
      // Calculate the areas of the three triangles formed by the point and each edge of the triangle
      val area1 = 0.5 * math.abs((b.x - a.x) * (p.y - a.y) - (p.x - a.x) * (b.y - a.y))
      val area2 = 0.5 * math.abs((c.x - b.x) * (p.y - b.y) - (p.x - b.x) * (c.y - b.y))
      val area3 = 0.5 * math.abs((a.x - c.x) * (p.y - c.y) - (p.x - c.x) * (a.y - c.y))
      val totalArea = 0.5 * math.abs((b.x - a.x) * (c.y - a.y) - (c.x - a.x) * (b.y - a.y))
      // Check if the sum of the areas of the three triangles equals the total area of the triangle
      area1 + area2 + area3 == totalArea
    def perimeter(s: Shape): Double = s match
      case Triangle(a, b, c) => distance(a, b) + distance(a, c) + distance(b, c)
      case Circle(c, r) => 2 * java.lang.Math.PI * r
      case Square(o, s) => 4 * s
    def contains(s:Shape, p: Point2D): Boolean = s match
      case Triangle(a, b, c) => pointInTriangle(p, a, b, c)
      case Circle(c, r) => pow(p.x - c.x, 2) + pow(p.x - c.y, 2) < pow(r, 2)
      case Square(o, s) => (p.x >= o.x && p.x <= o.x + s) && (p.y >= o.y && p.y <= o.y + s)


  import Shape.*
  println("EXERCISE 7")
  println("Expected: 5.65, actual: " + perimeter(Triangle(Point2D(0, 0), Point2D(1, 1), Point2D(2, 2))))
  println("Expected: 12.56, actual: " + perimeter(Circle(Point2D(0, 0), 2)))
  println("Expected: 16, actual: " + perimeter(Square(Point2D(0, 0), 4)))
  println("Expected: true, actual: " + contains(Triangle(Point2D(0, 0), Point2D(0, 4), Point2D(4, 0)), Point2D(2, 2)))
  println("Expected: false, actual: " + contains(Circle(Point2D(0, 0), 2), Point2D(4, 4)))
  println("Expected: true, actual: " + contains(Square(Point2D(0, 0), 2), Point2D(1, 1)))
  println()

  // exercise 8
  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:
    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) if f(a) => Some(a)
      case _ => None()
    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match
      case Some(a) => Some(f(a))
      case _ => None()
    def fold[A](opt: Option[A])(default: A)(f: A => A): A = opt match
      case Some(a) => f(a)
      case _ => default

  import Option.*
  println("EXERCISE 8, expected output: Some(5), None(), None()")
  println("Expected: Some(5), actual: " + filter(Some(5))(_ > 2))
  println("Expected: None(), actual: " + filter(Some(5))(_ > 8))
  println("Expected: None(), actual: " + filter(None[Int]())(_ > 2))
  println("Expected: Some(true), actual: " + map(Some(5))(_ > 2))
  println("Expected: Some(false), actual: " + map(Some(5))(_ > 8))
  println("Expected: None(), actual: " + map(None[Int]())(_ > 2))
  println("Expected: 6, actual: " + fold(Some(5))(1)(_ + 1))
  println("Expected: 1, actual: " + fold(None[Int]())(1)(_ + 1))
}
