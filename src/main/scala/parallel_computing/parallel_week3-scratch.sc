// Data parallelism
//    distribute the data across the computing nodes, instead of the tasks

// Parallel for loop
//   .par turns a range into a parallel range
//   relies on side effects - not functional in style

def initializeArray(xs: Array[Int])(v: Int): Unit = { // populate xs with v
    for (i <- 0 (until xs.length).par) {// need to either synchronize, or make sure the body is writing to separate memory locations
        xs(i) = v  // writing to separate memory locations, since each iteration covers a distinct array index i
    }
}

// performance of data parallel programs depends on workload - a function mapping input element to amount of work required to process it
//      the initializeArray function has a Uniform Workload - every iteration takes the same amount of work, so it's easy to parallelize
//      the data-parallel scheduler attempts to efficiently balance the workload across processor without knowing the shape of the Workload function


// Parallel collections
//   the functions of a parallel collection are executed in parallel

// multiples of 3 that are palindromes 
(1 until 1000).par
    .filter(n => n % 3 == 0)
    .count(n => n.toString == n.toString.reverse)
// filter and count are parallelizable operations

// some operations cannot be parallelizable
def foldLeft[B](z: B)(f: (B, A) => B): B
    // there is only one way to take prior inputs (A, B) of f and output B
    // Similarly, foldRight, reduceLeft, reduceRight, scanLeft, scanRight require previous inputs to produce output => can only be sequential

// Instead, use fold
def fold(z: A)(f: (A, A) => A): A
    // Inputs are of form (A, A), can be implemented in a reduction tree
    // require f to be associative, and z must be the identity of f
    // aka (f, z) must form a monoid over A 

// use fold to implement max
def max(xs: Array[Int]): Int = {
    xs.par.fold(Int.MinValue)(math.max)
}

// This does not compile - 0 is of a different type from the Array elements
Array('E', 'P', 'F', 'L').par
    .fold(0)((count, c) => if (isVowel(c)) count + 1 else count)

// Since fold requires f: (A, A) => A, while foldLeft can accept f: (A, B) => B
//   fold is less expressive than foldLeft - there is a tradeoff to gain parallelism
//  Resolve this using aggregate

def aggregate[B](z: B)(f: (B, A) => B, g: (B, B) => B): B
// f is sequential - split processing of f across multiple processors, on which f runs sequentially
// g is parallel - aggregates the parallel calls to f. forms a monoid with z

Array('E', 'P', 'F', 'L').par
    .aggregate(0)(
        f = (count, c) => if (isVowel(c)) count + 1 else count,
        g = _ + _
    )

//   Sequential collection data types have equivalent parallel data types, where the operations are performed in parallel
//   The sequential and parallel collection data type rolls up to the Generic data type

// for example
// Sequential type: Set
// Parallel type:   ParSet
// Generic type:    GenSet
def largestPalimdrome(xs: GenSeq[Int]): Int = {
    xs.aggregate(Int.MinValue)(
        (largest, n) =>
        if (n > largest && n.toString == n.toString.reverse) n else largest,
        math.max
    )
}

// Can pass either a Seq or ParSeq to this function

// Converting a sequential collection to a parallel one using .par
//   some types are easily convertible:
ParArray[T]
ParRange
ParVector[T]
immutable.ParHashSet[T]
immutable.ParHashMap[K, V]
mutable.ParHashSet[T]
mutable.ParHashMap[K, V]
ParTrieMap[K, V]
// for other collections, calling .par creates the closest parallel collection
// List.par => ParVector

def intersection(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
    val result = mutable.Set[Int]() // not threadsafe - different threads may end up conflicting
    // val result = new ConcurrentSkipListSet[Int]() // fix using a concurrent collection from java.util.concurrent._
    for (x <- a) if (b contains x) result += x 
    result
}

intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet) // yields correct output
intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet) // yields incorrect output


// alternatively, can fix using the correct combinator operators
def intersection(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
    if (a.size < b.size) a.filter(b(_))
    else b.filter(a(_))
}

// mutable map is not thread safe
val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i +> (i, i+1)) // Creates a circular graph
graph(graph.size - 1) = 0

for ((k, v) <- graph.par) graph(k) = graph(v) // collection is being modified while it's being traversed
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })

// can fix using TrieMap, which allows taking of snapshots to capture current state
val graph = concurrent.TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i+1))
graph(graph.size - 1) = 0

val previous = graph.snapshot()
for ((k, v) <- graph.par) graph(k) = previous(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })

// Avoid
//    - writing to a colleciton that is concurrently being traversed
//    - reading from a collection that is concurrently being modified


// data parallel methods - use the following abstractions
//   - iterators
//   - splitters
//   - builders
//   - combiners

trait Iterator[A] {
    def next(): A
    def hasNext: Boolean
} // Iterates over the collection using next()

trait Splitter[A] extends Iterator[A] { // parallel counterpart to iterator
    def split: Seq[Splitter[A]] // Recursive sequence of splitters to traverse the collection in parallel, disjointly
    def remaining: Int
}

trait Builder[A, Repr] { 
    // A - type of elements to add to the builder
    // Repr - type of collection the builder returns
    def +=(elem: A): Builder[A, Repr] // add an element to the builder
    def result: Repr // Return the resulting collection
}

trait Combiner[A, Repr] extends Builder[A, Repr] { // Parallel counterpart to builder
    def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
}

// construct methods using these abstractions

// foldLeft from iterator
extension (i: Iterator)
    def foldLeft[B](z: B)(f: (B, A) => B): B = {
        var result = z
        while (i.hasNext) {
            result = f(result, i.next())
        }
        result
    }

// fold from splitter
extension (s: Splitter)
    def fold(z: A)(f: (A, A) => A): A = {
        if (s.remaining < threshold) foldLeft(z)(f)
        else {
            val children = for (child <- s.split) yield task { child.fold(z)(f) }
            children.map(_.join()).foldLeft(z)(f)
        }
    }

// filter from builder
def filter(p: T => Boolean): Repr = {
    val b = newBuilder
    for (x <- this) if (p(x)) b += x
    b.result
}