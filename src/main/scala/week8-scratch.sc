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

def expr = 
    val x = { print("x"); 1 } // Assign 1 to x, and print "x" to log
    lazy val y = { print("y"); 2} // assign 
    def z = { print("z"); 3}
    z + x + z + y + x

expr == 3+1+3+2+1
// side effect of running this program:
//  print("x") at line 47 when 1 is assigned to x
//  print("z") at line 50, when function z is invoked
//  print("y") at line 50, when y is lazily evaluated
//  print("z") at line 50, when function z is invoked


// Expanding prime filter expression using LazyRange


lazyRange(1000 to 100000).filter(isPrime).apply(1) 
    == cons(1000, lazyRange(1000 + 1, 100000)).filter(isPrime).apply(1) // Definition of lazyRange, line 14

val C1 = cons(1000, lazyRange(1000 + 1, 100000)) // To simplify

C1.filter(isPrime).apply(1) == (if C1.isEmpty then C1
                                else if isPrime(C1.head) then cons(C1.head, C1.tail.filter(isPrime))
                                else C1.tail.filter(isPrime) // Expansion of filter
                                ).apply(1)
                            == (if isPrime(1000) then cons(C1.head, C1.tail.filter(isPrime))
                                else C1.tail.filter(isPrime) 
                                ).apply(1)
                            == (if false then cons(C1.head, C1.tail.filter(isPrime))
                                else C1.tail.filter(isPrime) 
                                ).apply(1)
                            == C1.tail.filter(isPrime).apply(1) // This continues until we get to the first prime
                            == lazyRange(1009, 100000).filter(isPrime).apply(1)
                            == cons(1009, lazyRange(1009 + 1, 100000)).filter(isPrime).apply(1)

val C2 = cons(1009, lazyRange(1009 + 1, 100000))

C2.filter(isPrime).apply(1) == cons(1009, C2.tail.filter(isPrime)).apply(1)
                            == cons(1009, C2.tail.filter(isPrime)).tail.apply(0) // by evaluation of apply
                            == C2.tail.filter(isPrime).apply(0) // This continues until we get to the second prime
                            == lazyRange(1013, 100000).filter(isPrime).apply(0)
                            == cons(1013, lazyRange(1013 + 1, 100000)).filter(isPrime).apply(0)

val C3 = cons(1013, lazyRange(1013 + 1, 100000))

C3.filter(isPrime).apply(0) == cons(1013, C3.tail.filter(isPrime)).apply(0)
                            == 1013

// The real implementation has a lazy head and lazy isEmpty
// Maintains a lazy state variable
class LazyList[+T](init: => State[T]):
    lazy val state: State[T] = init


enum State[T]:
    case Empty
    case Cons(hd: T, tl: LazyList[T])


