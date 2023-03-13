package u02

object ex3b extends App {

  val negLambda: (String => Boolean) => String => Boolean =
    f => s => !f(s)

  def negMethod(f: String => Boolean): String => Boolean =
    (s: String) => !f(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = negLambda(empty)

  println(notEmpty("foo") && !notEmpty(""))

}
