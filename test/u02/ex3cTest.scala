package u02

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import u02.exercises.ex3c.negMethod

class ex3cTest {

  @Test def testInt(): Unit =
    val positive: Int => Boolean = _ match
      case n if n >= 0 => true
      case _ => false
    val negative = negMethod(positive)

    assertTrue(positive(1) && negative(-1))

  @Test def testString(): Unit =
    val empty: String => Boolean = _ == ""
    val notEmpty = negMethod(empty)

    assertTrue(notEmpty("foo") && !notEmpty(""))
}
