// laziness - only computing when necessary, as late as possible

(1000 to 100000).filter(isPrime)(1) 
// Gets 2nd prime in the range
// Computationally intensive because it computes all primes first, then filters

// LazyList - similar to lists but elements only evaluated on demand

val xs = LazyList.cons(1, LazyList.cons(2, LazyList.empty))
// Or
val ys = LazyList(1, 2, 3)

// Use to construct lazyRanges
def lazyRange(lo: Int, hi: Int): LazyList[Int] = 
    if lo >= hi then LazyList.empty
    else LazyList.cons(lo, lazyRange(lo + 1, hi))

    // this returns a single object of type LazyList, instead of a range of objects

// instead of ::, write x #:: xs == LazyList.cons(x, xs)

// Implementation
// can think of as if head and isEmpty are computed when the LazyList is created, but tail is calculated when needed
// (Not actual implementation)
object TailLazyList:
    def cons[T](hd: T, tl: => TailLazyList[T]) = new TailLazyList[T]: //head hd is call-by-value, crucial difference is tail tl is call-by-name
        def isEmpty = false
        def head = hd
        def tail = tl
        override def toString = "LazyList(" + hd + ", ?)"

    val empty = new TailLazyList[Nothing]:
        def isEmpty = true
        def head = throw NoSuchElementException("empty.head")
        def tail = throw NoSuchElementException("empty.tail")
        override def toString = "LazyList()"

// Lazy evaluation
// The first time a a lazy object is evaluated, the result is stored
//    Therefore the next time it is called, it doesn't have to be re-evaluated

// Haskell is lazy evaluation by default
// Scala is strict evaluation by default, but can use lazy evaluation
lazy val x = List(1, 2, 3)