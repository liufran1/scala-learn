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

// Maps - iterables. Extend iterables of (key, value) pairs
// Map[Key, Value]

val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

// Maps are also functions. Map[Key, Value] extends the function type Key => Value
romanNumerals("V") // returns 5, throws an exception if the Key isn't already in the Map
romanNumerals.get("V") // Returns None if the Key isn't in the map
// Result of get is an Option

trait Option[+A]

case class Some[+A](value: A) extends Option[A]
object None extends Option[Nothing]
// map.get(key) returns
// * None if map does not contain the given key
// * Some(x) if map associates the given key with the value x
// Does not return Null - if any value can be Null, you never know beforehand whether a certain element is defined or not
// => Null was a mistake

// Can convert back to value's type using pattern matching
def showInt(numeral: String): Int = romanNumerals.get(numeral) match {
    case Some(ints) => ints
    case None => 0
}

// alternatively, can add default value
val romanNumerals1 = romanNumerals.withDefaultValue(0)

// Updating maps
// m + (k -> v) // The map that takes key k to value v and is otherwise equal to m
// m ++ kvs // The map m updated via + with all the key/value pairs in kvs
val m1 = Map("red" -> 1, "blue" -> 2)
val m2 = m1 + ("blue" -> 3)
m2 == Map("red" -> 1, "blue" -> 3)
m1

// sortWith - equivalent to orderBy in SQL
val fruit = List("apple", "pear", "orange", "pineapple")
fruit.sortWith(_.length < _.length) // Sort the list by the length of the strings ascending
fruit.sorted // uses the default sorting for the type

fruit.groupBy(_.head) == Map("p" -> List("pear", "pineapple"), "a" -> List("apple"), "o" -> List("orange"))
// _.head is the discriminator function

class Polynom(nonZeroTerms: Map[Int,Double]):{
    def this(bindings: (Int, Double)*) = this(bindings.toMap) // repeated parameters - vararg. the * represents a list

    val terms = nonZeroTerms.withDefaultValue(0.0)

    def + (other: Polynom): Polynom = {
        Polynom(other.terms.foldLeft(terms)(addTerm))
        // Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp)+coeff))) 
        // for the given exponent, get the coefficient and add to the other coefficient. If exp doesn't exist in the other Polynom, do nothing
    }
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
        val (exp, coeff) = term
        terms + (exp -> (terms(exp) + coeff))
    } // Helper for foldLeft version of + . More efficient since you don't need to build up the map

    override def toString = {
        val termStrings = {
            for (exp, coeff) <- terms.toList.sorted.reverse
            yield
                val exponent = if exp == 0 then "" else s"x^$exp"
                s"$coeff$exponent"
        if terms.isEmpty then "0"
        else termStrings.mkString(" + ")
        }
    }
}

val x = Polynom(0 -> 2, 1->-3, 2->1)

// Example - encoding mnemonics

class Coder(words: List[String]): {
    val mnemonics = Map(
        '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
        '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )
    /** Maps a letter to the digit it represents, inverts the map */
    private val charCode: Map[Char, Char] = {
        for
            (digit, str) <- mnemonics
            ltr <- str
        yield ltr -> digit // a specific binding in a map
    }

    private def wordCode(word: String): String = word.toUpperCase.map(charCode)

    private def wordsForNum: Map[String, List[String]] = words.groupBy(wordCode).withDefaultValue(Nil)

    def encode(number: String): Set[List[String]] = {
        if number.isEmpty then Set(Nil)
        else {
            for
                splitPoint <- (1 to number.length).toSet // Picking where to put a space to separate strings into words
                word <- wordsForNum(number.take(splitPoint)) // take all numbers up to the split point, call wordsForNum
                rest <- encode(number.drop(splitPoint)) // Recursively call for the remainder
            yield word :: rest
        }
    } 

}