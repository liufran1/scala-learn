// Parallel algorithms

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