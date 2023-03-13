package u02

object ex3a extends App {

  def positiveMethod(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  val positiveLambda: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positiveMethod(3))
  println(positiveMethod(-3))

  println(positiveLambda(3))
  println(positiveLambda(-3))

}
