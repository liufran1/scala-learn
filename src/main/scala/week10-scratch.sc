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


// Discrete Event simulation example
//   Digital circuit
//   * wires - connect components
//   * gates - components, transform signals carried by wires. there is a time delay to signals passing through gates
//      * Inverter
//      * AND
//      * OR
//   * signals - boolean values


// class Wire

val a = Wire()
val b = Wire()
val c = Wire()

// Components
def inverter(input: Wire, output: Wire): Unit // Create base components as side effects, don't return anything
def andGate(in1: Wire, in2: Wire, output: Wire): Unit
def orGate(in1: Wire, in2: Wire, output: Wire): Unit


// Half adder
def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit =
    // a, b are input wires
    // s, c are output wires
    val d = Wire()
    val e = Wire() // internal intermediate wires
    orGate(a, b, d)  // a and b both go into an OR gate
    andGate(a, b, c) // at the same time a and b both go into an AND gate
    inverter(c, e) // Take the output of the AND gate, and pass it to an inverter
    andGate(d, e, s) // AND the output of the inverted AND and the OR

    // result at s = !(a && b) && (a || b)

def f(a: Wire, b: Wire, c: Wire): Unit = 
    val d, e, f, g = Wire()
    inverter(a, d) // !a
    inverter(b, e) // !b
    andGate(a, e, f) // (a & !b)
    andGate(b, d, g) // (!a & b)
    orGate(f, g, c) // (a & !b) || (!a & b)

    // => a != b

//   Discrete event simulator: performs _actions_ at a given _moment_
//   * action: function that doesn't take any parameters and returns Unit
//   * time: simulated


// Simulations happen inside an object that inherits from the abstract class Simulation with the signature
// In every instance of the Simulation trait, maintain an "agenda" of actions to perform
//    List of Event pairs - action and the time when it must be produced
trait Simulation:
    private type Action = () => Unit
    private case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()
    private var curtime = 0


    def currentTime: Int = curtime // Current simulated time

    private def insert(ag: List[Event], item: Event): List[Event] = ag match{
        // ag - list of existing agenda items
        // item - Event to insert
        //    need to insert into the correct spot, based on time
        case first :: rest if first.time <= item.time => first :: insert(rest, item) // If first happens before item, move down the list
        case _ => item :: ag // If item happens before first, put item at the head of ag
    }

    def afterDelay(delay: Int)(block: => Unit): Unit = // Registers an action to perform after a certain delay relative to currentTime
        val item = Event(currentTime + delay, () => block) 
        agenda = insert(agenda, item)


    private def loop(): Unit = agenda match { // Event loop - pops events in succession off the agenda and runs them
        case first :: rest => { // Pull the first event off of the agenda
            agenda = rest
            curtime = first.time // Sets the current time to the time of first
            first.action() // Executes first
            // it's possible that this does not terminate, because an action might add additional actions to the agenda
            loop() // proceed through rest
        }
        case Nil => 
    }

    def run(): Unit = // Peforms the simulation until there are no more actions waiting
        afterDelay(0) {
            println(s"simulation started, time = $currentTime")
        }
        loop() // starts the event loop


end Simulation


trait Gates extends Simulation: // extend simulation
    
    def InverterDelay: Int
    def AndGateDelay: Int
    def OrGateDelay: Int

    class Wire:
        private var sigVal = false // Initially no current. can only access this using the getSignal and setSignal methods
        private var actions: List[Action] = List()

        def getSignal(): Boolean = sigVal // Returns the current value of the signal on the wire

        def setSignal(s: Boolean): Unit = // Sets the signal
            if s != sigVal then
                sigVal = s
                action.foreach(_()) // For each of the actions in the list actions, executes them all if the signal changes

        def addAction(a: Action): Unit = 
            actions = a :: actions // Adds an action to the list of actions
            a() // Executes the action immediately upon adding it to make the signal defined


    def inverter(input: Wire, output: Wire): Unit = 
        def invertAction(): Unit = 
            val inputSig = input.getSignal()
            afterDelay(InverterDelay) { output.setSignal(!inputSig) } // After a delay, set the signal to the opposite of the signal value
        input.addAction(invertAction) // Adds the invert action to the input wire


    def andGate(in1: Wire, in2: Wire, output: Wire): Unit = 
        def andAction(): Unit = 
            val in1Sig = in1.getSignal()
            val in2Sig = in2.getSignal()
            afterDelay(AndGateDelay) { output.setSignal(in1Sig & in2Sig)} // After a delay, set the signal to the AND
        in1.addAction(andAction)
        in2.addAction(andAction)


    def orGate(in1: Wire, in2: Wire, output: Wire): Unit = 
        def orAction(): Unit = 
            val in1Sig = in1.getSignal()
            val in2Sig = in2.getSignal()
            afterDelay(OrGateDelay) { output.setSignal(in1Sig | in2Sig)} // After a delay, set the signal to the AND
            // afterDelay(OrGateDelay) { output.setSignal(in1.getSignal() | in2.getSignal())} // Incorrect implementation. Getting the value of the signal after the delay, instead of before it
        in1.addAction(orAction)
        in2.addAction(orAction)

    // Alternative definition of or gate using inverters and AND gate - a || b == !(!a && !b)
    def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = 
        val notIn1, notIn2, notOut = Wire()
        inverter(in1, notIn1)
        inverter(in1, notIn1)
        andGate(notIn1, notIn2, notOut)
        inverter(notOut, output) 
        // Timing is going to be different because it inherits the delays from the other components instead of defining it itself
        // Because we have many more Events, split up, many of which can happen at the same time, they may conflict
        // Instead of having a single transactional event


    def probe(name: String, wire: Wire): Unit = // Add probe to circuit to get the signal
        def probeAction(): Unit = 
            println(s"$name $currentTime value = ${wire.getSignal()}")
        wire.addAction(probeAction)
end Gates



trait Circuits extends Gates:
    def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = // Same as above
        val d = Wire()
        val e = Wire()
        orGate(a, b, d)
        andGate(a, b, c)
        inverter(c, e) 
        andGate(d, e, s)

trait Delays:
    def InverterDelay = 2
    def AndGateDelay = 3
    def OrGateDelay = 5
// Define delays as a trait outside of Circuits

object sim extends Circuits, Delays // Concrete instantiation of a Circuit simulation


import sim._
val input1, input2, sum, carry = Wire()
probe("sum", sum)
probe("carry", carry)
halfAdder(input1, input2, sum, carry)

input1.setSignal(true)
run()
input2.setSignal(true)
run()

