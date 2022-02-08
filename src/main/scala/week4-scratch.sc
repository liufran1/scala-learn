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
