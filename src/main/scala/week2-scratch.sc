//Higher order functions: functions that take other functions as parameters
//First order functions act on simple data types

def sumInts(a: Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)

def cube(x: Int): Int = x * x * x

def sumCubes(a: Int, b: Int): Int = if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

//We define sum as taking the function f as an input so that we can take sums of different functions
//without having to rewrite the pattern
def sum(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)
}

def sumCube2(a: Int, b: Int): Int = sum(cube, a, b)

//f: Int => Int defines the function type

//Anonymous functions
//like literals for functions - 
//   don't have to write "def func ..." every time you write a new function you call once
//Syntactic sugar
// the anonymous function
// (x: Int) => x * x * x
// can be passed

def sumCube3(a: Int, b: Int) = sum(x => x * x * x, a, b)
// Don't need types on the 'x' because we already know it from the def of 'sum'


// Turning the above linear recursion version of sum into a tail-recursive version
// use accumulator acc
def sum1(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

//Currying

//A function returns another function
def sum2(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  }
  sumF
}

def sumCube4 = sum2(x => x * x * x)
sumCubes(1, 5)
sumCube4(1, 5)

//Can write this succintly
//the sum2 function applies to the cube function and returns a function,
//which is applied to the arguments
//Associativity works from left to right
sum2(cube)(1, 5)

//Shorter way of writing sum2
def sum3(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sum3(f)(a + 1, b)

//Sequentially nesting anonymous functions within each other is called "Currying"

def product(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 1 else f(a) * product(f)(a + 1, b)

def factorial(a: Int) = product(x => x)(1, a)
factorial(5)

//Abstracting product and sum gives us basically mapReduce, so we write it as so
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

//Syntax
//Extended Backus-Naur form (EBNF)
// | denotes an alternative
// [...] an option (0 or 1)
// {...} a repetition (0 or more)

//Data - Classes and Types

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
}

//Rational is the type
//Constructor creates elements of this type

//Objects are elements of a class
val x = new Rational(1,2) //Creating an object by calling the constructor
x.numer
x.denom

//Can use the type as a datatype
def addRational(r: Rational, s: Rational): Rational = {
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom
  )
}

//Can package functions into classes == methods

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer,denom)

  def sub(that: Rational) = add(that.neg)
}


class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")
  //require is a test
  //require enforces preconditions - if something is wrong, it's user error
  //assert is also a test
  //assert is a check on the function itself - if something is wrong, there is a bug in the function

  //The constructor here is implicit - the primary constructor
  
  //Can create a separate constructor - auxiliary constructor
  def this(x: Int) = this(x, 1)
  //This constructor takes an Int and passes it to the primary implicit constructor
  //Which sets numer = x and denom = 1

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b) 

  //private function called in the process of constructing the datatype
  //private means it can't be accessed outside of the class
  /*
  private val g = gcd(x,y)
  def numer = x / g
  def denom = y / g
  */
  //Can rewrite as val so that the gcd computation only happens once
  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)
  //Data abstraction - client observes same behavior regardless of implementation

  def less(that: Rational) = numer * that.denom < that.numer * denom

  //this refers to object on which the method is executed - self reference
  def max(that: Rational) = if (this.less(that)) that else this

  //calling simply numer is equivalent to this.numer

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer,denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = { // Classes come with a default toString method, so we need to specify that we're overriding it
    numer + "/" + denom 
  }
}

// Extension Methods - bolting on additional methods to a class outside of the class definition
// Can do for methods that don't need access to the internals of a class - can't refer to internals via this
// Makes the code more modular
extension (r: Rational)
  def min(s: Rational): Boolean = if (s.less(r)) then s else r
  def abs: rational = Rational(r.numer.abs, r.denom)


//Evaluation of objects is the same as evaluation for functions, that is call by value

//Operators
//all methods can be used as infix operators if they take a single parameter
// need to add 'infix def'
//x.add(x) == x add x

//Operators can be used as identifiers
//In Scala, you can use symbols, not just alphanumerics
//def < (that: Rational) = numer * that.denom < that.numer * denom

//Precedence determined by first character
//This defines order of operations
