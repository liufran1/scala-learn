//import sun.jvm.hotspot.runtime.amd64.AMD64CurrentFrameGuess
//Call by name vs call by value
def constOne(x: Int, y: => Int) = 1

//Scala by default does call by value.
//Call by value means the interpreter reduces function arguments to values before rewriting the function application
//
// => forces call by name

// constOne(3 * 4, loop)
// this evaluates 3 * 4 first, and then constOne(12, loop) since x is call by value
// if y were evaluated by call by value, this would not terminate since loop would keep trying to evaluate
// because y is forced to call by name, this returns 1 since y is unused in the function

// If call by value terminates, then call by name will as well. The opposite is not true


//Conditionals
def abs(x: Int) = if (x >= 0) x else -x

abs(-5)

def and(x: Boolean, y: => Boolean) = if (x) y else false
def or(x: Boolean, y:Boolean): Boolean = if (x) true else y
//Don't want y to always be evaluated

// Value definitions
// def creates a function that is defined by name
// val creates a function defined by value
val x = -2
val y = abs(x)
// y is immediately defined as 2


//Implementation of square root finder, v1
//Functional programming makes heavy use of recursion and breaking things up into small functions
object sqrtsession {
  def abs(x: Double) = if (x >= 0) x else -x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x)/x < 0.0001

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

}
sqrtsession.sqrt(0.1e-20)

//Implementation of square root finder, v2 using blocks
//Block refers to the brace delineation. Cleans things up by moving definitions within the scope of sqrt
//Due to scoping, eliminates need for passing x across functions. x is "global" within the scope of sqrt
object sqrtsessionNest {
  def abs(x: Double) = if (x >= 0) x else -x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }


}
sqrtsessionNest.sqrt(5)

//Recursion
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)
//Example of tail recursion. Function calls itself as its last action
//Uses constant space
//Avoids recursion depth -> stack overflow

gcd(35,80)

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)
//Not tail recursive since factorial is being multiplied by n, so there is a build up of space

def factorialTail(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n - 1)
  loop(1, n)
}

factorialTail(5)