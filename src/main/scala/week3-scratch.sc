//can wrap this in a package
//package week3
//just can't do it in a worksheet, has to be done in a class
// from another file: "import week3.intsets" for a specific item, or "import week3.*" for everything

object intsets {
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4
}

// Can contain members (methods) which are not implemented
abstract class IntSet {
  def incl(x: Int): IntSet //abstract member, it's not implemented yet
  def contains(x: Int): Boolean
  //Can leave undefined if using abstract class
  //Cannot instantiate an abstract class
  def union(other: IntSet): IntSet
}

//"Empty" and NonEmpty extend "IntSet"
//"Empty" and "NonEmpty" therefore conform to the type "IntSet"
// "IntSet" is the superclass of "Empty" and "NonEmpty"
// If no superclass given, then the standard class Object from java.lang is used
//"Object" is the standard class in both Java and Scala 

// immediate and subsequent superclasses are base classes
// -> IntSet and Object are base classes of NonEmpty

class Empty extends IntSet {
  def contains(x: Int): Boolean = false // We now implement the abstract member contains from IntSet
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other
  override def toString: String = "."
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = { // Add x to the set
    if (x < elem) new NonEmpty(elem, left incl x, right) // Search left
    else if (x > elem) new NonEmpty(elem, left, right incl x) // Search right
    else this // Found in this set
  }

  def union(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  override def toString: String = "{" + left + elem + right + "}"

}

// Alternatively, can define a singleton object for Empty
// There is really only one empty set - we don't expect to instantiate it more than once
// no other instance of Empty can be created

object Empty extends IntSet:
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
end Empty

// Can create an Object and Class with the same name
// There are two global namespaces: types and values
// classes are in the "type" namespace, while objects are in the "value" one
// A class and object in the same sourcefile with the same name: "companions"
// Like a static class - add methods that exist once per class, instead of once per class instance


//Programs
//Standalone applications contain an object with a "main" method
object Hello {
  def main(args: Array[String]) = println("hello")
}
//calling "scala Hello" from the command line will then print "hello"

//dynamic method dispatch
//analogous to calls to higher order functions
//code invoked by a method call depends on the runtime type of the object that contains the method
//"Empty contains 1" will evaluate to false
//On the other hand, "(new NonEmpty(7, Empty, Empty)) contains 7" evaluates to true
//the call to the method "contains" depends on the object calling it

//classes can only have one superclass
//"single inheritance"
//to get around it, use traits
//declared like an abstract class
trait Planar {
  def height: Int
  def width: Int
  def surface = height * width
}

//then can combine as follows
//class Square extends Shape with Planar with Movable
//traits can't have value parameters

//null is a subtype of value type

//Polymorphism - function can be applied to arguments of many types

//Immutable linked list
//fundamental data structure in many functional languages
//Two building blocks
//"Nil" - empty list
//"Cons" - cell containing an element and the remainder of the list

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// Type parameter [T] means we can take multiple types

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}
//"val" is evaluated first time the object is initialized
//vs "def" which is evaluated each time it is referenced

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
//compiler can infer argument type, so type parameter can be omitted, as below
singleton(true)

//In scala, type parameters don't affect evaluation, basically removed before evaluation of the program
//"type erasure"

//Types of polymorphism:
//Subtyping: instances of subclass can be passed to base class
//Generics: instances of a function or class are created by type parameterization

def nth[T](n: Int, list: List[T]): T = {
  if (list.isEmpty ) throw new IndexOutOfBoundsException("Index out of bounds")
  else if (n == 0) list.head
  else nth[T](n - 1, list.tail)
}


// Scala is a pure object-oriented language
// Every function can be represented as an object