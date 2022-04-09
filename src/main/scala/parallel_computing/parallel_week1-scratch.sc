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

