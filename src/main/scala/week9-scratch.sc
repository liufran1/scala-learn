// Type Directed Programming

val x = 42 // Scala infers that the type of x is Int, because the type of 42 is Int

// Conversely, Scala can infer values from types

// Abstracting over contexts
//   Example - when sorting, want to be able to sort for multiple types
//   For each type (Int, String, Etc) this requires a comparison < operator that is specific for that type
//   Can abstract that comparison operator, and pass to the sort function as a parameter

def sort[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = ???

// results in
sort(ints)((x, y) => x < y)

scala.math.Ordering[A] // standard library already has a class to represent orderings

// So we can instead parametrize with the ordering object

def sort[T](xs: List[T])(ord: Ordering[T]): List[T] = ???

sort(ints)(Ordering.Int)

// It would be nice for the compiler to pick the appropriate Ordering object automatically
// Use implicit parameter - prefix with using (Scala 3) - (implicit in Scala 2)

def sort[T](xs: List[T])(using ord: Ordering[T]): List[T] = ???

sort(strings)(using Ordering.String) // Can either include the using argument, or leave it out
sort(ints)

// Compiler infers types from values - Type Inference
// Compiler infers expressions/terms from types - Term Inference
//   When there is exactly one "obvious" value for a type in a "using" clause, the compiler can provide it automatically

// Can include multiple using clauses
def f(x: Int)(using a: A, b: B) = ???
def f(x: Int)(using a: A)(using b: B) = ???
f(x)(using a, b) // Both a and b are using parameters
f(x)(using y) // y is matched to a, b is left out. compiler matches from left to right

def f(x: Int)(using a: A)(y: Boolean)(using b: B) = ???
f(x)(using a)(y)(using b)

// Parameters of using clauses can be anonymous
def sort[T](xs: List[T])(using Ordering[T]): List[T] = ???
// useful if sort doesn't use ord itself, but needs to pass it to further methods
// like merge
def merge[T](xs: List[T])(using Ordering[T]): List[T] = ???

// Context bounds - analogous to variable bounds
def printSorted[T](as: List[T])(using Ordering[T]) = println(sort(as))
    // can instead write
def printSorted[T: Ordering](as: List[T]) = println(sort(as)) //"There must be an Ordering for the variable type T"


// Given instances
// for something to be used in a using clause, it must be defined with given (Scala 3) - (implicitly in Scala 2)
object Ordering:
    given Int: Ordering[Int] with // a given instance of type Ordering[Int], named Int
        def compare(x: Int, y: Int): Int = if x < y then -1 else if x > y then 1 else 0

// Can define given instances anonymously
    given Ordering[Double] with // The compiler will synthesize a name for this - given_Ordering_Double. Sometimes these clash
        def compare(x: Int, y: Int): Int = ???

// Summon an instance
summon[Ordering[Int]] // Expands to Ordering.Int
def summon[T](using x: T) = x

//    For an implicit parameter of type T, compiler will search for a given instance that
//        * has a type compatible with T
//        * is visible a the point of the function call
//              * inherited
//              * imported
//              * defined in an enclosing scope
//        * Or defined in a companion object associated with T
//              * companion ojbects associated with any of T's inherited types
//              * companion ojbects associated with any type argument in T
//              * if T is an inner class, the outer objects in which it is embedded
//        * if there is a single most specific instance, it will be used
//        * if not, an error is thrown
trait Foo[T]
trait Bar[T] extends Foo[T]
trait Baz[T] extends Bar[T]
trait X
trait Y extends X
// If a given instance of type Bar[Y] is required, the compiler will look at
//  * Bar
//  * Y
//  * Foo
//  * X
// it will not look at Baz


// Importing
// * By-name
import scala.math.Ordering.Int
// * By-type - the preferred method
import scala.math.Ordering.{given Ordering[Int]}
import scala.math.Ordering.{given Ordering[?]} // ? is a wildcard
// * Wildcards
import scala.math.given // Imports all given types


// Ambiguity
//   More than one eligible given instance
trait C:
    val x: Int
given c1: C with
    val x = 1
given c2: C with
    val x = 2

def f(using c: C) = ()
f // raises an ambiguity
// both value c1 and value c2 match type C of parameter c of method f
// can resolve this by passing the argument explicitly
f(using c2)

// This doesn't happen if one given instance a is more specific than another b
//    * a is in a closer lexical scope than b
//    * or a is defined in a class or object which is a subclass of the class defining b
//    * or type A is a generic instance of type B
//    * or type A is a subtype of type B

class A[T](x: T)
given universal[T](using x: T): A[T](x) with {}
given specific: A[Int](2) with {}

summon[A[Int]] // summons specific

//
trait A:
    given ac: C
trait B extends A:
    given bc: C
object O extends B:
    val x = summon[C] // summons bc

//
given ac : C
def f() = 
    given b : C
    def g(using c: C) = ()

g // summons b


// Type classes

trait Ordering[A]:
    def compare(x: A, y: A): Int

object Ordering:
    given Ordering[Int] with
        def compare(x: Int, y: Int): Int = if x < y then -1 else if x > y then 1 else 0

    given Ordering[String] with
        def compare(s: String, t: String) = s.compareTo(t)

// Ordering is a "type class"
//    A generic trait with given instances for type instances of that trait
//    At compilation time, the compiler resolves the specific Ordering implementation that matches the type being used

//    This supports retroactive extension - can add the "compare" capability to Int and String without changing the data types themselves

// Can also implement conditional instances

given listOrdering[A](using ord: Ordering[A]) as Ordering[List[A]] with // Takes type parameter A and implicit parameter ord
    def compare(xs: List[A]) = (xs, ys) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => 1
        case (x :: xs1, y :: ys1) => 
            val c = ord.compare(x, y)
            if c != 0 then c else compare(xs1, ys1)
    }
// a listOrdering for a type T only exists if an Ordering exists for T
// should be implemented within a type class

// Given instances with implicit parameters are resolved recursively

def sort[A](xs: List[A])(using Ordering[A]): List[A] = ???
val xss: List[List[Int]] = ???

sort(xss) == sort[List[Int]](xss)
          == sort[List[Int]](xss)(using listOrdering)
          == sort[List[Int]](xss)(using listOrdering(using Ordering.Int))

// Example: sorting addresses
//    Sort first by zipcode, and then by streetname
type Address = (Int, String) // Zipcode, Street Name
val xs: List[Address] = ???

// implement ordering
given pairOrdering[A, B](using orda: Ordering[A], ordb: Ordering[B]): Ordering[(A, B)] with
    def compare(x: (A, B), y: (A, B)) = 
        val c = orda.compare(x._1, y._1)
        if c != 0 then c else ordb.compare(x._2, y._2)


// Type class traits may define extension methods, and these extension methods are made available when summoned

trait Ordering[A]:
    def compare(x: A, y: A): Int

    extension (x: A)
        def < (y: A): Boolean = compare(x, y) < 0
        def <= (y: A): Boolean = compare(x, y) <= 0
        def > (y: A): Boolean = compare(x, y) > 0
        def >= (y: A): Boolean = compare(x, y) >= 0

// Can use type classes to implement abstract algebra concepts
trait SemiGroup[T]:
    extension (x: T) def combine (y: T): T // The binary operator is associative. 
    // If a Semigroup has an identity, it is a monoid
    // A monoid with invertibility is a group

def reduce[T: SemiGroup](xs: List[T]): T = xs.reduceLeft(_.combine(_))

// extending to Monoid
trait Monoid[T] extends SemiGroup[T]:
    def unit: T

def reduce[T](xs: List[T])(using m: Monoid[T]): T = xs.foldLeft(m.unit)(_.combine(_))
// rewrite using context bounds, use summon to avoid having to call m directly
def reduce[T: Monoid](xs: List[T]): T = xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))

// Can streamline this further by defining a global function Monoid.apply[T] that returns the Monoid[T] instance that is currently visible
object Monoid:
    def apply[T](using m: Monoid[T]): Monoid[T] = m

def reduce[T: Monoid](xs: List[T]): T = xs.foldLeft([Monoid[T]].unit)(_.combine(_))