// Task Parallelism

// Parallel merge sort
def sort(from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth) {
        quickSort(xs, from, until - from) // Don't parallelize at given depth
    }
    else {
        val mid = (from + until) / 2
        parallel(sort(mid, until, depth + 1), sort(from, mid, depth + 1)) // Split array in middle

        val flip = (maxDepth - depth) % 2 == 0
        val src = if (flip) ys else xs
        val dst = if (flip) xs else ys // alternate which array is the base one that gets merged into
        merge(src, dst, from, mid, until)
    }
}


// Parallel map
//   can use map with lists, although lists are not the best for parallel computations
//     can't split lists efficiently - need to search for the middle
//     can't combine lists efficiently - concatenation takes linear time

// Sequential version of map on Lists
def mapSeq[A,B](lst: List[A], f: A => B): List[B] = lst match {
    case Nil => Nil
    case h :: t => f(h) :: mapSeq(t, f) // recursive
}

// Sequential version of map on Arrays
def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
    // write results to out
    var i = left
    while (i < right) { // while loop on iter i
        out(i) = f(inp(i))
        i = i+1
    }
}

// Turn this into a parallel form by dividing and conquering
def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
    // write results to out
    if (right - left < threshold)
        mapASegSeq(inp, left, right, f, out)
    else {
        val mid = left + (right - left) / 2
        parallel(mapASegPar(inp, left, mid, f, out), // Need to be careful that the indices don't overlap
                 mapASegPar(inp, mid, right, f, out))
    }
}


// Using trees, implement parallel map
def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(a) => {
        val len = a.length
        val b = new Array[B](len)
        var i = 0
        while (i < len) { // arrays in Leafs are small enough not to parallelize
            b(i) = f(a(i))
            i = i+1
        }
        Leaf(b)
    }
    case Node(l, r) => {
        val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
        Node(lb, rb)
    }
}

// Arrays vs Trees
//  Arrays
//    + random access to elements
//    + good memory locality
//    - imperative, must ensure parallel tasks write to disjoint parts
//    - expensive to concatenate

//  Trees
//    + purely functional - immutable, produce new ones and keep old ones
//    + no need to worry about keeping parallel tasks disjoint
//    + combining trees is efficient
//    - high memory allocation overhead
//    - bad locality

// Parallel fold / reduce

// Using trees - sequential reduce
def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => f(reduce[A](l, f), reduce[A](r, f))
}
// if f is non-associative, then the structure of the tree matters
// In order for reduce to work, need f to be associative
//   f can be non-commutative 
//   - many floating point operations are not associative, like addition and multiplication
//       this is because it is easier to make an operation commutative than it is to make it associative
//       can patch functions to be commutative by looking at the ordering of the inputs
def f(x: A, y: A) = if x < y then g(y, x) else g(x, y)

// Using trees - parallel reduce
def reduce[A](t: Tree[A], f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Node(l, r) => {
        val (lV, rV) = parallel(reduce[A](l, f), reduce[A](r, f))
        f(lV, rV)
    }
}
// Depth complexity is linear in the height of the tree


// Using arrays - parallel reduce

def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A, A) => A): A = {
    if (right - left < threshold) {
        var res = inp(left)
        var i = left + 1
        while (i < right) {
            res = f(res, inp(i))
            i = i + 1
        }
    }
    else {
        val mid = left + (right - left) / 2
        val (a1, a2) = parallel(reduceSeg(inp, left, mid, f), 
                                reduceSeg(inp, mid, right, f))
        f(a1, a2)
    }
}

def reduce[A](inp: Array[A], f: (A, A) => A): A = reduceSeg(inp, 0, inp.length, f)



// Prefix Sum algorithm

// scan - like a cumumlative operation
List(1, 3, 8).scanLeft(100)((s, x) => s + x) == List(100, 101, 104, 112) // each entry is the cumulative sum going from left to right, starting with the accumulator
List(1, 3, 8).scanRight(100)(_ + _) == List(112, 111, 108, 100) // go from right to left

def scanLeft[A](inp: Array, // input array
                a0: A, // accumulator
                f: (A, A) => A // function
                out: Array[A] // output array
            ): Unit = {
    out(0) = a0
    var a = a0
    var i = 0
    while (i < inp.length) {
        a = f(a, inp(i))
        i = i + 1
        out(i) = a
    } // At first glance, cannot parallelize this because each entry depends on the previous one
} // Can parallelize by computing f more than inp.length times - speedups from parallelism makes up for recomputations


// Start by expressing scanLeft using map and reduce

def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A

def mapSeg[A, B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B, out: Array[B]): Unit

def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val fi = (i: Int, v: A) => reduceSeg1(inp, 0, i, a0, f) // define a function which is the reduction of f up until the index i
    mapSeg(inp, 0, inp.length, fi, out) // map fi to index i, write it to out
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last)) // get the last element of out, since out is longer than inp by one
}

// Implement using trees

// Input - store values in leaves
sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

// Intermediate values tree - store res values in nodes as well
sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

// Define reduce using these trees - turn an input Tree into an output TreeRes
def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
        val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
        NodeRes(tL, f(tL.res, tR.res), tR) // res value for node is f applied to res of its children
    }
}

// parallel version - can perform the recursive calls in parallel
// call the function "upsweep"

def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(l, r) => {
        val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
        NodeRes(tL, f(tL.res, tR.res), tR)
    }
}

// Example:
List(1, 3, 8, 50).scanLeft(100)((s, x) => s + x) == List(100, 101, 104, 112, 162)
//  f = (s, x) => s + x
//   input tree:
//            *
//          /   \
//         *     *
//        / \   / \
//       1  3  8  50

// after upsweep pass
//   intermediate tree:
//            62
//          /    \
//         4     58
//        / \    / \
//       1  3   8  50

// next need to perform downsweep pass

def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(a) => Leaf(f(a0, a))
    case NodeRes(l, _, r) => {
        val (tL, tR) = parallel(downsweep[A](l, a0, f), // When going down the left subtree, pass the accumulator a0
                                downsweep[A](r, f(a0, l.res), f)) // When going down the right subtree, pass the result of applying f to a0 and the left subtree
        Node(tL, tR)
    }
}

// after downsweep pass
//   output tree:
//               62+100
//          /              \
//         4+100         58+104
//        /   \          /    \
//      1+100  3+104  8+104  50+112


// final step: prepend the accumulator
def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(l, r) => Node(prepend(x, l), r)
}


// We therefore define scan using upsweep and downsweep
def scanLeft[A](t: Tree[A], a0, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, a0, f)
    prepend(a0, scan1)
}


// Implementation of prefix sum using arrays
//  still store intermediate results with trees

sealed abstract class TreeResA[A] { val res: A }
case class Leaf[A](from: Int, to: Int, // Add additional indices representing the input array's indices
                   override val res: A) extends TreeResA[A]

case class Node[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]


def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA = {
    if (to - from < threshold) {
        Leaf(from, to, reduceSeg1(inp, from + 1, to, inp(from), f)) // Below threshold, apply sequential reduce
    }
    else {
        val mid = from + (to - from) / 2
        val (tL, tR) = parallel(upsweep(inp, from, mid, f), 
                                upsweep(inp, mid, to, f))
        Node(tL, f(tL.res, tR.res), tR)
    }
}


def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
    var a = a0
    var i = left
    while (i < right) {
        a = f(a, inp(i))
        i = i+1
    }
    a
}

def scanLeftSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A, out: Array[A]) = {
    if (left < right) {
        var i = left
        var a = a0
        while (i < right) {
            a = f(a, inp(i))
            i = i + 1
            out(i) = a
        }
    }
}

def downsweep[A](inp: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
    case Leaf(from, to, res) => scanLeftSeg(inp, from, to, a0, f, out)
    case Node(l, _, r) => {
        val (_, _) = parallel(downsweep(inp, a0, f, l, out), 
                              downsweep(inp, f(a0, l.res), f, r, out))
    }
}


def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val t = upsweep(inp, 0, inp.length, f)
    downsweep(inp, a0, f, t, out)
    out(0) = a0 // prepends a0
}