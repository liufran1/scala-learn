// Data decomposition
// Given data, how do you figure out what's in it

// Object oriented decomposition - add accessor methods to every class
//    It couples data and operations - need to touch all classes to add a new method
// Functional decomposition - reverse the construction process by getting at
//    Which subclass was used
//    The arguments used

// Case Classes

trait Expr
object Expr: {
    case class Number(n: Int) extends Expr
    case class Sum(e1: Expr, e2: Expr) extends Expr
    case class Var(name: String) extends Expr
    case class Prod(e1: Expr, e2: Expr) extends Expr    
}

// Use pattern matching to access the members of the class
def  eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
}
// A selector expression, followed by a sequence of cases of form pat => expr
// Each case associates an expression expr with a pattern pat
// MatchError is thrown if no pattern matches the value of the selector e

def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)}" + s"${show(e2)}"
    case Var(x) => x
    case Prod(e1, e2) => s"${showP(e1)}" + s"${showP(e2)}"
}


def showP(e: Expr): String = e match {
    case e: Sum => s"(${show(e)})"
    case _ => show(e)
}


// Lists
// Compared to arrays:
//    Lists are immutable - can't be changed after they've been created
//    Lists are recursive, arrays are flat
//       The head of a list is its entry, its tail is the next list representing the rest of the list
//       List(1,2,3,4) is constructed as
//       1 :: (2 :: (3 :: (4 :: Nil)))
// Lists are homogeneous - all elements must have the same type
// three operations / methods
//    head
//    tail
//    isEmpty

// Can use pattern matching to decompose lists
//    1 :: 2 :: xs - Lists that start with 1, then 2
//    x :: Nil or List(x) - 1 element lists 

// Insertion sort
def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
}
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if x < y then x :: xs else y :: insert(x, ys)
}


// Enums
// Class hierarchies are bundles of functions operating on common values represented as fields
// Composing and decomposing pure data without associated functions - class hierarchies are overkill
// Instead - case classes and pattern matching

// Pure data definitions without methods - Algebraic Data Types (ADTs)

// Enum - shorthand for case class hierarchies
enum Expr: {
    case class Number(n: Int) // Avoids the "extends Expr" notation from above
    case class Sum(e1: Expr, e2: Expr)
    case class Var(name: String)
    case class Prod(e1: Expr, e2: Expr)
}

// Can define enum cases without parameters
enum Color:
    case Red
    case Green
    case Blue

// Or: 
enum Color:
    case Red, Green, Blue

def notRed(color: Color) = color match {
    case Green | Blue => true
    case _ => false
}

// useful for domain modelling tasks where you need to define a large number of data types without attaching operations


// Bounding types
def assertAllPos[S <: IntSet](r: S): S = ???
// "<: IntSet" is an upper bound of the type parameter S
// So the accepted types are all subtypes of IntSet (Empty and NonEmpty)
// This is superior to 
// def assertAllPos(s: IntSet): IntSet
// because it is clear that inputs of type Empty return type Empty, and inputs of type NonEmpty return type NonEmpty

// Can conversely write lowerbounds
// [S >: NonEmpty] - S can range over supertypes of NonEmpty
// Or mix bounds
// [S >: NonEmpty <: IntSet]

// Covariance
// Given NonEmpty <: IntSet, does this imply
// List[NonEmpty] <: List[IntSet]
// if this holds, then we say List is "covariant"

// Liskov principle - If A <: B, then everything one can do with a value of type B
// one should be able to do with a value of type A

// A type that accepts mutations of its elements should not be covariant
// immutable types can be covariant, given certain conditions

// Given A <: B, C[T]
// C[A] <: C[B] C is covariant. Can annotate as C[+A]{...}
// C[A] >: C[B] C is contravariant. Can annotate as C[-A]{...}
// else C is nonvariant. Can annotate as C[A]{...}

// If A2 <: A1 and B1 <: B2, then
// A1 => B1 <: A2 => B2
// -> functions are contravariant in their argument types and covariant in their result type
// the scala compiler does variance checks roughly according to this rule

// Can define Lists using these concepts

trait List[+T]:
    def isEmpty() = this match {
        case Nil => true
        case _ => false
    }

    override def toString = {
        def recur(prefix: String, xs: List[T]): String = xs match {
            case x :: xs1 => s"${prefix}${x}${recur(",", xs1)}"
            case Nil => ")"
        }
        recur("List(", this)
    }

    case class ::[+T](head: T, tail: List[T]) extends List[T]
    case object Nil extends List[Nothing]

    extension [T](x: T) 
        def :: (xs: List[T]): List[T] = ::(x, xs)

    object List:
        def apply() = Nil
        def apply[T](x: T) = x :: Nil
        def apply[T](x1: T, x2: T) = x1 :: x2 :: Nil
        // etc

    def prepend[U >: T](elem: U): List[U] = ::(elem, this) 
    // Need U >:T so that this method is contravariant to List[+T]
    // Example of use of lower bounds
