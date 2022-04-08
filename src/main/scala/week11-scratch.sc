// Functional reactive programming - event based programs typically created imperatively can be created functionally
//   not pure functional programming

// Observer pattern
//   variants known as
//     * publish / subscribe
//     * model / view / controller
//   views react to changes in a model

//  Pros
//     * decouples views from state
//     * can set up varying number of views of a given state
//     * simple to set up - easy to convert stateful objects to this
//  Cons
//     * forces an imperative style, since handlers are of type Unit
//     * need to coordinate many moving parts
//     * concurrency adds complexity
//     * views are tightly bound to one state


trait Subcriber:
    def handler(pub: Publisher): Unit

trait Publisher:
    private var subcribers: Set[Subcriber] = Set() // maintain a set of subscribers

    def subscribe(subscriber: Subcriber): Unit = subcribers += suscriber // add to the subscribers set

    def unsubscribe(subscriber: Subcriber): Unit = subcribers -= suscriber // remove from the subscribers set

    def publish(): Unit = subcribers.foreach(_.handler(this)) // for each subscriber, call its handler


// Example: BankAccount

class BankAccount extends Publisher:
    private var balance = 0
    def currentBalance: Int = balance

    def deposit(amount: Int): Unit = 
        if amount > 0 then
            balance = balance + amount
            publish()

    def withdraw(amount: Int): Unit = 
        if 0 < amount && amount <= balance then
            balance = balance - amount
            publish()
        else throw Error("insufficient funds")

// Consolidator maintains the total balance of a list of accounts
class Consolidator(observed: List[BankAccount]) extends Subcriber:
    observed.foreach(_.subscribe(this))

    private var total: Int = _ // initialized by the call to compute. the = _ syntax indicates this is an uninitialized variable (as opposed to abstract variable)
    compute()

    private def compute() = total = observed.map(_.currentBalance).sum

    def handler(pub: Publisher) = compute()
    def totalBalance = total


//   Functional reactive programming seeks to address the downsides of the observer model
//   It aggregates an event sequence into a "signal"
//     * Signal: a value that changes over time
//     * f: time -> value
//     * define new signals in terms of existing ones (instead of propagating updates to a mutable state)

// Event-based way to describe a mouse move
MouseMoved(toPos: Position) // fires an event

// FRP alternative
mousePosition: Signal[Position] // function that describes position at a given point in time


// Fundamental Signal Operations
//   * Obtain value of the signal at the current time
mousePosition()
//   * Define a signal in terms of other signals
def inRectangle(LL: Position, UR: Position): Signal[Boolean] = 
    Signal {
        val pos = mousePosition()
        LL <= pos && pos <= UR
    }


// Computing signals
//   * can evaluate on demand, whenever a value is needed
//   * can be sampled at certain points in time, and interpolated 
//   * discrete signal could be broadcast everytime it's updated - functional analogue to the observer pattern

val sig = Signal(3) // signal that has a constant value of 3. can't be updated
val sig = Signal.Var(3) // signal that has a constant value of 3, but can be updated using the update operation
sig.update(5) // sig is now updated to 5
sig() = 5 // alternative syntax

val y = Signal(sig * 2) // initially we would get 6 (3*2) for the value of y
// when sig() = 5 is called, the value of y is automatically updated to 10

// Version of BankAccount that uses signals
class BankAccount:
    private val myBalance = Signal.Var[Int](0) // This is now a signal var, instead of var

    def balance: Signal[Int] = myBalance // new method to retrieve the balance

    def currentBalance: Int = balance

    def deposit(amount: Int): Unit = 
        if amount > 0 then
            val b = myBalance()
            myBalance() = b + amount // update the signal myBalance() instead of publishing

    def withdraw(amount: Int): Unit = 
        if 0 < amount && amount <= balance() then
            val b = myBalance()
            myBalance() = b - amount
            myBalance()
        else throw Error("insufficient funds")


def consolidated(accts: List[BankAccount]): Signal[Int] = 
    Signal(accts.map(_.balance()).sum)


val a = BankAccount()
val b = BankAccount()

val c = consolidated(List(a,b))

a.deposit(10)
c() == 10

b.deposit(20)
c() == 30

a.withdraw(8)
c() == 22


// Implementing Signal and Var

// Each signal maintains
//   * its current value
//   * the current expression that defines the signal value
//   * a set of observers - the other signals that depend on its value. they need to be reevaluated when the signal changes
//   Recording dependencies
//      * when evaluating a signal-valued expression, need to know which other signal gets defined or updated (the caller)
//      * executing a sig() update means adding caller to the observers of sig
//      * when sig's value changes, all previously observing signals are re-evaluated
//         * the set sig.observers is cleared
//         * the set is cleared because re-evaluation of sig will re-enter the signal caller into the list sig.observers, as long as caller still depends on sig

trait Signal[+T]:
    def apply(): Signal.Observed[T]

object Signal:


    opaque type Observer = AbstractSignal[?]
    

    abstract class AbstractSignal[+T] extends Signal[T]:
        private var currentValue: T = _
        private var observers: Set[Observer] = Set()

        def apply(): Observed[T] = 
            observers += caller // Add callers to observer set
            assert(!caller.observers.contains(this), "cyclic signal definition") // Prevents scenarios where you attempt to define s() = s() + 1
            currentValue // Return current value

        protected def eval: () => Observed[T] // Evaluate the signal


// Signal value is evaluated using computeValue()
//   * on initialization
//   * when an observed signal changes its value
        protected def computeValue(): Unit = 
            val newValue = eval(using this)

            val observeChange = observers.nonEmpty && newValue != currentValue // check there are observers, and the newValue represents a change

            currentValue = newValue // update the currentValue
            if observeChange then
                val obs = observers
                observers = Set() // clear observers. computeValue will re-insert each observer
                obs.foreach(_.computeValue()) // computeValue for each observer


    def apply[T](expr: => Observed[T]): Signal[T] = 
        new AbstractSignal[T]:
            val eval = () => expr
            computeValue()

    class Var[T](initExpr: => Observed[T]) extends AbstractSignal[T]:
        protected var eval = () => initExpr
        computeValue()

        def update(newExpr: => Observed[T]): Unit = 
            eval = () => newExpr
            computeValue()

    // Need way to determine the relevant callers. Could pass the caller to every expression that is evaluated
    // Can do this by passing them implicitly
    type Observed[T] = Observer ?=> T

    def caller(using o: Observer) = o

    given noObserver as Observer = new AbstractSignal[Nothing]: // For case where there are no signals that are callers, pass this implicitly
        override def eval = ???
        override def computeValue() = ()