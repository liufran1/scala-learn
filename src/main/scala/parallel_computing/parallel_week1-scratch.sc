// Parallel computing
//   Power wall - at a point, increase in computational power doesn't scale with an increase in processor frequency

//   Challenges of parallel computing
//      Separating computations into parallel subcomputations can be challenging
//      Ensuring correctness is harder, there are more points of failure

//   Parallelism vs Concurrency
//      Parallelism - divide a computation into subproblems. execute computation more quickly
//      Concurrency - modularizing computation => orchestration, information exchange, managing shared resources


//   Parallelism granularity
//      bit-level - processing multiple bits of data in parallel. on the hardware level
//      instruction-level - executing different instructions from the same instruction stream in parallel. on the hardware level
//      task-level - executing separate instruction streams in parallel. at the software level


//   Parallel hardware
//      multi-core processors - processor with multiple cores on one chip
//      symmetric multiprocessors - multiple chips sharing memory
//      GPUs - need to be called for specific programs
//      field-programmable gate arrays - rewires itself for a given task
//      computer clusters - connected by a network, no shared memory

//   Focus on JVM runtime on multicore / multiprocessor systems with shared memory

//   Operating system - software that manages hardware and software resources, schedules program execution
//   Process - an instance of a program that is executing in the OS. 
//             Assigned a PID. 
//             Can run the same program in multiple processes simultaneously. 
//             OS multiplexes many different processes and a limited number of CPUs. they are given time slices of execution => multitasking
//             Memory is not shared across processes
//   Threads - independent concurrency units within a process. share memory. each thread has a 
//             program counter - describes position in the current method
//             program stack - region of memory. contains sequence of method invocations that are currently being executed. In the JVM, the stack is only accessible by the thread that owns the stack. in order to communicate, threads need to modify shared heap memory


// When a JVM process starts, it starts several threads including a main thread which executes the main method
//   For parallel computations, need to start additional threads by
//     1. Defining a Thread subclass
//     2. Instantiate a new Thread object 
//     3. Call start on the Thread object

class HelloThread extends Thread {
    override def run() {
        println("Hello") // Code to run within the thread
        println("World")
    }
}

val t = new HelloThread
val s = new HelloThread

t.start() // start a new thread in parallel to main
s.start()
t.join() // stops the main thread until HelloThread completes
s.join()

// The order in which the statements can be different, since t and s are parallel threads
// Sometimes want to enforce some sequence
// -> Atomicity: an operation completes all at once. As if it occurred instantly from the point of view of other threads

private var uidCount = 0L

def getUniqueId(): Long = {
    uidCount = uidCount + 1 // Not atomic, so different threads will print overlaping uids
    uidCount // Another thread could be getting uidCount before the other is updating its assignment, so the two return the same uid
}

def startThread = {
    val t = new Thread {
        override def run {
            val uids = for (i <- 0 until 10) yield getUniqueId() 
            println(uids)
        }
    }
    t.start()
    t
}

// Can enforce atomicity using the synchronized block
private val x = new AnyRef {} // Synchronized needs to be invoked on some object, so we create x just to hold it

private var uidCount = 0L
def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
}

// Deadlocking
class Account(private var amount: Int = 0){
    def transfer(target: Account, n: Int) =
        this.synchronized {
            target.synchronized { // Nest the synchronization blocks
                this.amount -= n  // Ensure atomicity on both the initiating and target account
                target.amount += n
            }
        }
}

def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
        override def run() {
            for (i <-0 until n) {
                a.transfer(b, 1)
            }
        }
    }
    t.start()
    t
}

val a1 = new Account(500000)
val a2 = new Account(700000)

val t = startThread(a1, a2, 150000)
val s = startThread(a2, a1, 150000)
t.join()
s.join()

// This never terminates
//   t and s are both competing for ownership of the monitors on the a1 and a2 accounts, 
//     because we set up a synchronization block around the account transfers
//   t and s are waiting for each other to finish without releasing the already acquired resources
//     t has grabbed the monitor block around a2, s has grabbed a1. t is waiting for s to finish with a1 to grab it, and vice-versa

// Ways to resolve deadlocks -
//   Always acquire resources in the same order, so it's not cyclic

val uid = getUniqueUid()

private def lockAndTransfer(target: Account, n: Int) = 
    this.synchronized {
        target.synchronized {
            this.amount -= n
            target.amount += n
        }
    }
def transfer(target: Account, n: Int) = 
    if (this.uid < target.uid) this.lockAndTransfer(target, n) // Ensures transfers always happen ordered by account uid
    else target.lockAndTransfer(this, -n)

//   Memory model - set of rules on how threads interact when accessing shared memory
//     * Two threads writing to separate location in memory do not need synchronization
//     * a thread X that calls join on thread Y is guaranteed to observe all the writes by thread Y after join returns



// Calculating p-norm in parallel
def sumSegment(a: Array[Int], p: Int, s: Int, t: Int): Int = { // Sequential sum
    var i = s
    var sum: Int = 0
    while (i < t) {
        sum = sum + power(a(i), p)
        i = i + 1
    }
    sum
}

def pNormRec(a: Array[Int], p: Int): Double = 
    power(segmentRec(a, p, 0, a.length), 1/p)

def segmentRec(a: Array[Int], p: Int, s: Int, t: Int) = {
    if (t - s < threshold) then sumSegment
    else {
        val m = s + (t - s)/2 // Split indices in two to recursively break into parallel computations
        val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t)) // The parallel call indicates these should be run in parallel
        sum1 + sum2
    }
}// parallel - a function that takes parameters by name


// task - alternative construct
val (v1, v2) = parallel(e1, e2) 
// Can alternatively be written as
val t1 = task(e1) // starts e1 in the background
val t2 = task(e2)
val v1 = t1.join // blocks and waits until e1 has completed
val v2 = t2.join

def task(c: => A): Task[A] // takes c by name

trait Task[A] {
    def join: A
}

// Can define parallel in terms of task
def parallel[A, B](cA: => A, cB: => B): (A, B) = {
    val tB: Task[B] = task { cB } // kicks B into a task while A is computed directly
    val tA: A = cA // no need to put A into a task because we're not computing anything else
    (tA, tB.join)
}


// Asymptotic complexity analysis
//   Work W(e): number of steps e would take if there were no parallelism. sequential execution time
//     W(parallel(e1, e2)) = W(e1) + W(e2) + c2 (constant cost)
//   Depth D(e): number of steps e would take with unbounded parallelism - max of parallel running times
//     D(parallel(e1, e2)) = max(D(e1), D(e2)) + c1

//  given P parallel threads, an estimate of running time is
//  D(e) + W(e)/P
//    W(e)/P - need to do every piece of work, can only reduce this by spreading across the P threads
//    D(e) - have dependencies at every depth


// Amdahl's law - given
//   p: proportion of the program that benefits from speedups
//   s: improvement in speed
//  then overall speedup = 1/(1-p+p/s)
// so if 30% of a program benefits from a 2x speedup, the overall speedup is 1/(1-0.3+0.3/2) = 1.18

// Benchmarking programs
//   measurement is difficult because the performance metric is usually a random variable
//   need to measure in steady state, so often need to perform a warm-up

//   ScalaMeter - existing library
import org.scalameter._

val time = measure {
    (0 until 100000).toArray
} // Returns time in milliseconds

// JVM Warmup consists of the following steps
//   1. Program is interpreted
//   2. Program is compiled
//   3. JVM applies dynamic optimizations
//   4. Program reaches steady state

//  ScalaMeter - Warmer objects run the benchmarked code until detecting steady state

val time = withWarmer(new Warmer.Default) measure {
    (0 until 100000).toArray
}

// Can also specify parameters over the default
val time = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.verbose -> true,

) withWarmer(new Warmer.Default) measure {
    (0 until 100000).toArray
}

// Can also measure other metrics such as memory footprint and garbage collection cycles
withMeasurer(new Measurer.MemoryFootprint) measure {
    (0 until 100000).toArray
}