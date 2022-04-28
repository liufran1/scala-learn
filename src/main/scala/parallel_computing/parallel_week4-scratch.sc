// Implementing data parallel data structures

// concatenation of arrays and union of sets take linear time due to the need to copy entries into consecutive blocks of memory
// => for most data structures, combiner can not be implemented efficiently

// Two phase construction - have an intermediate data structure that has an efficient combine method, and efficient += method
//    combine is O(log(n) +log(m))
//    conversion to the resulting data structure is O(n/P)

class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) {
    private var numElems = 0 // Stores number of elements in the combiner
    private val buffers = new ArrayBuffer[ArrayBuffer[T]] // nested array to store elements
    buffers += new ArrayBuffer[T] // Elements stored in the entries of the nested array
}


def +=(x: T) = {    // finds the last ArrayBuffer in the parent array, and appends x to it
    buffers.last += x // if the array gets full, it is expanded to accomodate more elements
    numElems += 1
    this
}


def combine(that: ArrayCombiner[T]) = {
    buffers ++= that.buffers // copies the references of that's buffers to this's buffers
    numElems += that.numElems
    this
}


def result: Array[T] = {
    val array = new Array[T](numElems) // instantiate result array
    val step = math.max(1, numElems / parallelism)
    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)  // create a chunk for each parallel processor
    val tasks = for ((from, end) <- chunks) yield task {
        copyTo(array, from, end) // on each processor, copy the array buffer elements into the array
    }
    tasks.foreach(_.join())
    array
}
// This is efficient because it partitions the input into discrete partitions

// Conc-Trees
//   data type for efficient concatenation

//   lists with :: operator are built for sequential operations - :: recursively traverses the list from left to right
//   trees are suited for parallel computations, as we can traverse the subtrees in parallel
//      the trees need to be balanced for them to be suitable

// Need to store additional information at each node 
// conc-list data type
sealed trait Conc[+T] {
    def level: Int // longest path from root to leaf, the height
    def size: Int // number of elements in the subtree
    def left: Conc[T]
    def right: Conc[T]
}

case object Empty extends Conc[Nothing] { 
    def level = 0
    def size = 0
}

class Single[T](val x: T) extends Conc[T] { // Analogous to Leafs
    def level = 0
    def size = 1
}

case class <>[T] (left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
}
// we disallow <> from containing Empty as a subtree
// we enforce abs(left.level - right.level) <= 1 // balance criterion

def <>(that: Conc[T]): Conc = { // constructor method for the class
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that) // concat needs to reorganize the tree to enforce it's balanced
} // the complexity of this is proportional to the height difference in the two trees

def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys) // if things are already balanced, then just link them
    else if (diff < -1) { // left tree goes deeper
        if (xs.left.level >= xs.right.level) { // left tree is left-leaning
            val nr = concat(xs.right, ys) // peel off the right subtree and concat it with ys
            new <>(xs.left, nr) // then add back the left subtree
        }
        else { // left tree is right leaning
            val nrr = concat(xs.right.right, ys)
            if (nrr.level == xs.level - 3) { // recursively concat the right-most sub-trees
                val nl = xs.left
                val nr = new <>(xs.right.left, nrr)
                new <>(nl, nr)
            }
            else {
                val nl = new <>(xs.left, xs.right.left)
                val nr = nrr
                new <>(nl, nr)
            }

        }
    }
    else { // right tree goes deeper - implementation is analogous
        ???
    }
}


// Can now use conc-trees to implement a combiner

var xs: Conc[T] = Empty
def +=(elem: T) {
    xs = xs <> Single(elem) // runs in O(log(n)) as we insert elem into the proper position
}

// can get to O(1) by extending the Conc-Tree data structure and relaxing some of its restraints
case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
} // don't impose the balance criterion

// one way to implement appendLeaf
def appendLeaf[T](xs: Conc[T], y: T): Conc[T] = Append(xs, new Single(y))
// however, this adds a new Append node for every element that is added
// in order to convert this back to a balanced tree, need to traverse each newly appended element in linear time

def appendLeaf[T](xs: Conc[T], ys: Single[T]): Conc[T] = xs match {
    case Empty => ys // trivial case - return ys if xs is empty
    case xs: Single[T] => new <>(xs, ys) // trivial case - concat two Leafs together
    case _ <> _ => new Append(xs, ys) // straightforward - if xs is not an Append node, link ys using a new Append node
    case xs: Append[T] => append(xs, ys)
}

@tailrec private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) new Append(xs, ys)
    else {
        val za = new <>(xs.right, ys)
        xs.left match {
            case ws @ Append(_, _) => append(ws, zs)
            case ws if ws.level <= zs.level => ws <> zs
            case ws => new Append(ws, zs)
        }
    }
}


// ConcBuffer - a combiner, appends elements into an array of size k

class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
    private var chunk: Array[T] = new Array(k)
    private var chunkSize: Int = 0
} // fills up an array. once the array is full, push it into the conc tree and create new empty array

// add elements to the chunk array
final def +=(elem: T): Unit = {
    if (chunkSize >= k) expand() // once the array becomes full, use expand to push it to the conc tree
    chunk(chunkSize) = elem
    chunkSize += 1
}


// Create new Chunk nodes - like Single leaf nodes but hold an array instead of a single element
class Chunk[T](val array: Array[T], val size: Int) extends Conc[T] {
    def level = 0
}


private def expand() {
    conc = appendLeaf(conc, new Chunk(chunk, chunkSize)) // append new Chunk node into the conc tree
    chunk = new Array(k) // reinitialize new array
    chunkSize = 0 // reset the counter
}

final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    val combinedConc = this.result <> that.result // get conc-trees from the conc buffers and combines them
    new ConcBuffer(k, combinedConc)
}

def result: Conc[T] = {
    conc = appendLeaf(conc, new Conc.Chunk(chunk, chunkSize))
    conc
}