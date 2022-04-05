// Functions and state
//   Moving away from the pure functional approach

// in the lambda calculus, the Church Rosser Theorem states that the order in which simplifications are applied does not change the end result. key principle of functional programming

// Conversely - objects have state if their behavior is influenced by their history

// Variables instead of Values
var x: String = "abc" // this is mutable. can be changed later through an assignment
var count = 111

x = "hi"
count = count + 1

// Example - non-empty lazy lists using a mutable variable
def cons[T](hd: T, tl: => TailLazyList[T]) = new TailLazyList[T]:
    def head = hd
    private var tlOpt: Option[TailLazyList[T]] = None
    def tail: T = tlOpt match {
        case Some(x) => x
        case None => tlOpt = Some(tl); tail // if tail is empty, update it and call tail recursively until we've filled it as needed
    }

// Referential transparency 
val x = E

// when it is clear that the following are equivalent, we have referential transparency. it is clear that the two expressions are the same
val y = E
val y = x 

// Operational equivalence - x and y are operationally equivalent if no test can distinguish them
// if E is stateful, this breaks

// Loops
//   Can do while-do as a higher order function
def whileDo(condition: => Boolean)(command: => Unit): Unit =
    if condition then
        command
        whileDo(condition)(command)
    else ()

//   For loop - combine for statement with do expression
for i <- 1 until 3 do System.out.print(s"$i ")
// translates using foreach
(1 until 3).foreach(i => System.out.print(s"$i "))