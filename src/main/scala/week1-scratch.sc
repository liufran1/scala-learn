import sun.jvm.hotspot.runtime.amd64.AMD64CurrentFrameGuess

//Conditionals
def abs(x: Int) = if (x >= 0) x else -x

abs(-5)

def and(x: Boolean, y: => Boolean) = if (x) y else false
//Don't want y to always be evaluated


//Implementation of square root finder, v1
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
//Due to scoping, eliminates need for passing x across functions
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