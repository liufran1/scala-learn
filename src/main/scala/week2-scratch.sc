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
//like literals for functions
//Syntactic sugar
// the anonymous function
// (x: Int) => x * x * x
// can be passed

def sumCube3(a: Int, b: Int) = sum(x => x * x * x, a, b)

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