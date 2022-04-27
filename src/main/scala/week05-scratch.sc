// Scala lists
// Immutable - don't change in-place, create a new list from old list

// Type
List[String]

// Construction - two equivalent ways
val list1 = List("s", "t", "c", "o", "o", "l")
val list2 = "s" :: "t" :: "c" :: "o" :: "o" :: "l" :: Nil

// decomposition
list1.head
list1.tail
list1.isEmpty

// Decompose using pattern match
list1 match {
    case x :: y :: _ => (x,y) //Extracts first two entries into a tuple
}

// List methods

list1.length // Number of elements
list1.last // Last element - analogous to head
list1.init // Everything except .last - analogous to tail
list1.take(n) // First n elements - whole list if length < n
list1.drop(n) // List after removing the first n elements
list1(n) // Element at index n

// Creating new lists
list1 ++ list2 // List concat
list1.reverse // reverse order
list1.updated(n, x) // Change element at index n to x

// finding elements
list1.indexOf(x) // First index of x. element not found returns -1
list1.contains(x) == list1.indexOf(x) >= 0


// last runs in linear time since you need to go through the whole list to get there
def last(T)(xs: List[T]): T = xs match {
    case List() => throw Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
}

// similarly for init
def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
}

extension [T](xs: List[T])
    def ++ (ys: List[T]): List[T] = xs match {
        case Nil => ys
        case x :: xs1 => x :: (xs1 ++ ys)
    }

extension [T](xs: List[T])
    def reverse: List[T] = xs match {
        case Nil => Nil
        case y :: ys => ys.reverse ++ List(y) // Inefficient since you keep reversing the tail
    }

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case y :: ys => {
        if n == 0 then ys
        else y :: removeAt(n - 1, ys) 
    } // Basically keep adding until you get to the index, then append tail of that sublist
}

def flatten(xs: Any): List[Any] = xs match {
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)
    case _ => xs :: Nil
} // Turn nested lists into a single list

// Merge sort implementation
def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if n == 0 then xs
    else {
        def merge(xs: List[Int], ys: List[Int]) = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) =>  xs
            case (x :: xs1, y :: ys1) => {
                if x < y then x :: merge(xs1, ys) 
                else y :: merge(xs, ys1)
            } // Can generalize and make this polymorphic by allowing an additional parameter for the comparison operator

        }
        val (fst, snd) = xs.splitAt(n) // returns two halves of xs split at n as a pair
        merge(msort(fst), msort(snd))
            }
    }
}

extension [T](xs: List[T])
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))

// Higher order list functions - Map, filter, and Reduce
extension [T](xs: List[T])
    def map[U](f: T => U): List[U] = xs match {
        case Nil => xs
        case x :: xs => f(x) :: xs.map(f)
    } // Map - apply function f to every element of list xs


extension [T](xs: List[T])
    def filter[T](p: T => Boolean): List[T] = xs match {
        case Nil => xs
        case x :: xs => if p(x) then x :: xs.filter(p) else xs.filter(p)
    } // Keep only elements that satisfy function p


// Variations on filter
list1.filterNot(p) == list1.filter(!p)
list1.partition(p) == (list1.filter(p), list1.filterNot(p)) // Single traversal, so more efficient than calling filter and filterNot
list1.takeWhile(p) // Traverse list and keep adding elements that satisfy p, at the first instance p is not satisfied, stop
list1.dropWhile(p) // Remainder of list after takeWhile is applied
list1.span(p) == (list1.takeWhile(p), list1.dropWhile(p))

val nums = List(1, 2, 3, 4, 5, 6)

nums.partition(x => x % 2 != 0) == (List(1, 3, 5), List(2, 4, 6))
nums.span(x => x % 2 != 0) == (List(1), List(2, 3, 4, 5, 6))


// Example: Pack
def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
        val (more, rest) = xs1.span(y => y == x)
        (x :: more) :: pack(rest)
    }
} // Packs consecutive duplicates into sublists

pack(List("a","a","a","b","c","c","a")) == List(List("a","a","a"), List("b"), List("c","c"), List("a"))

// Example: run length encoding
def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs).map(x => (x.head, x.length))
} // Takes the pack and gets the count of each sublist

encode(List("a","a","a","b","c","c","a")) == List(("a",3), ("b",1), ("c",2), ("a",1))


// Reduce
// Combine all elements of a list

def sum(xs: List[Int]) = (0 :: xs).reduceLeft((x,y) => x + y) // Adds all elements in the list
// can more precisely represent using _ notation. Each _ represents a new parameter going left to right
def sum(xs: List[Int]) = (0 :: xs).reduceLeft(_ + _) // Adds all elements in the list

// FoldLeft is the more general form - takes an accumulator z as a parameter for when foldLeft is called on an empty list
def sum(xs: List[Int]) = xs.foldLeft(0)(_ + _) // Adds all elements in the list

// foldLeft applies the operation by associating to the left - for sum: ((((0 + 1) + 2) + 3) + ...)
// Can equivalently define foldRight and reduceRight
// Different results from the two if the operation is either not associative or commutative


abstract class List[T]:
    def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
        case Nil => z
        case x :: xs => xs.foldLeft(op(z, x))(op)

    def foldRight[U](z: U)(op: (T, U) => U): U = this match {
        case Nil => z
        case x :: xs => op(x, xs.foldRight(z)(op))
    }


// Reimplement reverse using foldLeft
def reverse[T](xs: List[T]): List[T] = xs.foldLeft(List[T]())((xs, x) => x :: xs)
// Using List[T]() instead of Nil because the current version of scala can't properly infer that Nil is of type List[T]
// This is linear in the length of xs, instead of quadratic


// Implement map and length using foldRight instead of foldLeft
def mapFun[T, U](xs: List[T], f: T => U): List[U] = xs.foldRight(List[U]())((y, ys) => f(y) :: ys)

def lengthFun[T](xs: List[T]): Int = xs.foldRight(0)((y, n) => n + 1)


// Structural induction
//    Prove a property P(xs) holds for all lists xs
//        * Show that P(Nil) holds - base case
//        * Show that if P(xs) holds, the P(x :: xs) holds - induction step

// Example: concat is associative
//    (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

// Base case
//    (Nil ++ ys) ++ zs == (ys) ++ zs
//                      == ys ++ zs
//                      == Nil ++ (ys ++ zs)

// Induction step
//    (x::xs ++ ys) ++ zs == (x::(xs ++ ys)) ++ zs
//                        == x::((xs ++ ys) ++ zs)
//                        == x::(xs ++ (ys ++ zs)) // By induction hypothesis
//                        == x::xs ++ (ys ++ zs)
