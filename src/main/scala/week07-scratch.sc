// Queries with for
// For is analogous to a query languages


val books: List[Book] = List(
    Book(title = "Title 1", authors = List("Last1, First1", "Last2, First2"))
)

for // Get titles of books written by author whose last name is Last1
    b <- books
    a <- b.authors
    if a.startsWith("Last1,")
yield b.title


// for translates using the methods map, flatMap, and withFilter
// as long as those methods are defined, can define for

// Functional Random Generators
// Systematic way to get random values for data of type T
trait Generator[+T]:
    def generate(): T

val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate() = rand.nextInt()

val booleans = new Generator[Boolean]:
    def generate() = integers.generate() > 0

val pairs = new Generator[(Int, Int)]:
    def generate() = (integers.generate(), integers.generate())

// Instead of writing this boilerplate, use map and flatMap

extension [T, S](g: Generator[T])
    def map(f: T => S) = new Generator[S]:
        def generate() = f(g.generate())

    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generate() = f(g.generate()).generate()

// Can now use a "for" structure

// val booleans = for x <- integers yield x > 0
//              = integers.map(x => x > 0)
//              = new Generator[Boolean]: 
//                 def generate() = ((x: Int) => x > 0)(integers.generate())
//                                = integers.generate() > 0

// def pairs[T, U](t: Generator[T], u: Generator[U]) = t.flatMap(x => u.map(y => (x, y)))
//                                                   = t.flatMap(x => new Generator[(T, U)]{ 
//                                                                                             def generate() = (x, u.generate())
//                                                                                         })
//                                                   = new Generator[(T, U)]: 
//                                                         def generate() = (new Generator[(T, U)]{ 
//                                                                                             def generate() = (t.generate(), u.generate())
//                                                                                         }).generate()
//                                                   = new Generator[(T, U)]:
//                                                         def generate() = (t.generate, u.generate())
                                                        
// Other generator examples
def single[T](x: T): Generator[T] = new Generator[T]:
    def generate() = x

def range(lo: Int, hi: Int): Generator[Int] = 
    for x <- integers yield lo + x.abs % (hi - lo)

def oneOf[T](xs: T*): Generator[T] = 
    for idx <- range(0, xs.length) yield xs(idx)


val choice = oneOf("red", "green", "blue") // A generator on a list of strings
choice.generate() // Every call returns one of the list members randomly


// Random list generator
def lists: Generator[List[Int]] = 
    for
        isEmpty <- booleans // Flip a coin on whether return empty or not. can change this distribution by using range
        list <- if isEmpty then emptyLists else nonEmptyLists
    yield list

def emptyLists = single(List())

def nonEmptyLists = 
    for
        head <- integers
        tail <- lists // Recursively call the generator again
    yield head :: tail


// Random tree generator
def trees: Generator[Tree] = 
    for
        isLeaf <- booleans
        tree <- if isLeaf then leafs else inners
    yield
        tree

def leafs = 
    for x <- integers yield Tree.Leaf(x)

def inners = 
    for x <- trees; y <- trees yield Tree.Inner(x, y)


// can use random generators to generate random test inputs
def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for i <- 0 until numTimes do
        val value = g.generate()
        assert(test(value), s"test failed for $value")
    println(s"passed $numTimes tests")
} // Run test for numTimes


test(pairs(lists, lists)) {
    (xs, ys) => (xs ++ ys).length > xs.length
} // This will print that it fails for some values, because the property doesn't hold for empty lists
// This testing paradigm means you can write the property you assume holds, and test it with some random values

// The ScalaCheck testing library automatically identifies generators
forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 + l2).size // forAll is part of ScalaCheck and automatically identifies that List generators are required
}


// Monads
// Data structures with map, and flatMap along with a set of algebraic laws

// monad M is a parametric type M[T] with two operations, flatMap and unit

extension[T, U](m: M[T])
    def flatMap(f: T => M[U]): M[U] // AKA bind 

    def unit[T](x: T): M[T]

// Many existing data structures are monads
// List is a monad with unit(x) = List(x)
// Set is a monad with unit(x) = Set(x)
// Option is a monad with unit(x) = Some(x)
// Generator is a monad with unit(x) = single(x)

// map can be defined as a combination of flatMap and unit

// m.map(f) == m.flatMap(x => unit(f(x))) // map x to f(x) and wrap it in a singleton list that flatMap then flattens
//          == m.flatMap(f andThen unit)

extension [A, B, C](f: A => B)
    infix def andThen(g: B => C): A => C = x => g(f(x)) // function composition

// Monad laws
// Associativity
m.flatMap(f).flatMap(g) == m.flatMap(f(_).flatMap(g))

// Left unit
unit(x).flatMap(f) == f(x)

// Right unit
m.flatMap(unit) == m


// Associativity means we can inline nested for expressions
// for 
//     y <- for x <- m; y <- f(x) yield y
//     z <- g(y)
// yield z

// ==

// for x <- m; y <- f(x); z <- g(y)
// yield z

// Right unit means
// for x <- m yield x == m


// Exceptions with Monads

// try/catch
def validatedInput(): String =
    try getInput()
    catch
        case BadInput(msg) => println(msg); validatedInput()
        case ex: Exception => println("error, aborting"); throw ex

// exception is caught by the closest enclosing catch handler that matches its type

// drawbacks to exceptions
// * don't show up in the types of functions that throw them
// * don't work in parallel computations where we want to communicate an exception from one thread to another

// Try - like option but with Success and Failure
abstract class Try[+T]
case class Success[+T](x: T) extends Try[T]
case class Failure(ex: Exception) extends Try[Nothing]


object Try:
    def apply[T](expr: => T): Try[T] = 
        try Success(expr)
        catch case NonFatal(ex) => Failure(ex)
// used to pass between threads and processes the results of computations that can fail with an exception

for 
    x <- computeX
    y <- computeY
yield f(x, y)

// if computeX and computeY succeed with results Success(x) and Success(y), this returns Success(f(x, y))
// if either fails with exception ex, this returns Failure(ex)

// not a Monad because the left unit law fails
// however, instead we are guaranteed to never throw a nonfatal exception