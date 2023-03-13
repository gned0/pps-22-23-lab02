package u02

object ex3c {

  def negMethod[A](f: A => Boolean): A => Boolean =
    (s: A) => !f(s)

}
