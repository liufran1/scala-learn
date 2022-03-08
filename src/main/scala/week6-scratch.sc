// Vectors
// Store elements in a tree
val nums = Vector(1, 2, 3, -88)


// Same operations as Lists, except for ::
// Have +: and :+ instead

x +: xs // new vector with leading element x, followed by all elements of xs
xs :+ x // new vector with trailing element x, preceded by all elements of xs

// Vector and List are both subclasses of Seq - class of sequences
// Seq is a subclass of Iterable

// Arrays and Strings support same operations as Seq, and can implicitly be converted to sequences when needed
// Not subclasses of Seq because they come from Java

val xs: Array[Int] = Array(1, 2, 3)
xs.map(x => 2 * x)

val ys: String = "Hello World!"
ys.filter(_.isUpper)

// Range - sequence of evenly spaced Integers
// Object with three fields: lower bound, upper bound, step value
// Operators: until, to, by

val r: Range = 1 until 5 // Does not include 5
val s: Range = 1 to 5 // Includes 5
val t: Range = 1 to 10 by 3

// Sequence operations
r.exists(p) // true if there is an element for which p holds
r.forall(p) // true if p holds for all elements
r.zip(t) // pairs of elements with the same index from each of r and t
r.unzip // Splits a sequence of pairs into two sequences corresponding to each element of the pairs
r.flatMap(f) == r.map(f).flatten // Applies f t all elements and concatenates the results. Equivalent to calling 
r.sum // Sum of all elements
r.product // Product of all elements
r.max // Max of all elements
r.min // Min of all elements

// All combinations of numbers x and y, where x is from 1 to M, and y is from 1 to N
(1 to M).flatMap(x => (1 to N).map(y => (x,y))) 

// Inefficient Prime number check 
def isPrime(n: Int): Boolean = (2 until n).forall(n % _ != 0)

// Nested sequences
// for positive integer n, find all pairs of positive integers i and j, 1 <= j < i < n, such that i + j is prime
(1 until n).flatMap(i => (1 until i).map(j => (i,j))).filter((x,y) => isPrime(x + y))

// For expression
// For s yield e
case class Person(name: String, age: Int)

persons.filter(p => p.age > 20).map(p => p.name) // Names of all persons over age 20
for p <- persons if p.age > 20 yield p.name // Equivalent.

// Generator: p <- persons - p is a pattern and persons is an expression whose value is a collection
// Filter: if p.age > 20 - a boolean expression

// Prime pair revisited
for 
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
yield (i, j)


// Scalar product
def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for (x, y) <- xs.zip(ys) yield x * y).sum
}

// Sets
val s = Set(1, 2, 3, 4 ,5) 

// Most sequence operations are available for sets
// Unordered
// No duplicate elements
// fundamental operation is contains
s.contains(5)

// N-queens problem
// Place N queens on a chessboard in a way such that no queen threatens another

// Recursive algorithm
//  * have generated all solution for k-1 queens, on board of size n
//  * Each solution is represented by a list of length k-1 containing the numbers of columns, 0 to n-1
//  * Column number of the queen in the k-1th row comes first in the list, followed by the column number of the queen in row k-2, and so on
//  * The solution set is represented as a set of lists, with one element for each solution
//  * to place the kth queen, we generate all possible extensions of each solution preceded by a new queen
def queens(n: Int) = {

    def placeQueens(k: Int): Set[List[Int]] = { // k: number of queens remaining to be placed
        def isSafe(col: Int, queens: List[Int]): Boolean = !checks(col, 1, queens)
        def checks(col: Int, delta: Int, queens: List[Int]): Boolean = queens match {
            case qcol :: others => {
                qcol == col // Vertical check
                || (qcol - col).abs == delta // Diagonal check
                || checks(col, delta + 1, others)
            }
            case Nil => false
        }


        if k == 0 then Set(List())
        else
            for
                queens <- placeQueens(k - 1)
                col <- 0 until n
                if isSafe(col, queens)
            yield col :: queens
    }
    placeQueens(n)
}