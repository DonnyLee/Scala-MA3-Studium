
/**
 * A simple singly-linked immutable list, parameterized with the type of its elements.
 * An empty SimpleListPolymorphic is simply an object.
 * A non-empty SimpleListPolymorphic is a ListNode; actually, since a ListNode references its neighbor, 
 * it is more like a chain of ListNodes: A typical recursive data structure. The element
 * contained in the ListNode is called the head of the list (i.e. its first element). 
 * The other ListNodes (i.e. the rest of the list's elements) collectively form another
 * SimpleListPolymorphic, called the tail of the original list.
 * 
 * This is not the "main solution" - see the comment at the beginning of the "main solution" SimpleList.scala.
 * 
 * @author peine
 */

/**
 * An abstract class that defines the API of SimpleList and also provides a few
 * method implementations. 
 * (For those returning later (after are traits are treated in this course) to this code
 * and then wondering why SimpleList is an abstract class and not a trait: It is an 
 * abstract class and not a trait only because with a trait, the companion object SimpleList
 * below could not have a main method.)
 */

abstract class SimpleListPolymorphic[+Elem] {
          /*
           * Elem is a type parameter for the element type.
           * +Elem declares SimpleList[Elem] to be covariant with respect to Elem: 
           * This ensures that a List[Other] is a subtype of List[Elem] if Other 
           * is a subtype of Elem. For example, List[Student] is a subtype of List[Person] then.
           * Such a subtype relationship is type-safe if and only if the List is immutable.
           * 
           * You can safely ignore that "+" if you don't need the subtype property. Covariance of 
           * type parameters will not be treated in this course (only covariance of method 
           * parameter types will be treated in the context of the object-oriented paradigm)
           */

  /**
   * An internal helper class storing the head and the tail of a
   * non-empty SimpleListPolymorphic. It also has all method implementations
   * for non-empty SimpleListPolymorphic's.
   */
  private class ListNode[Elem](hd: Elem, tl: SimpleListPolymorphic[Elem]) extends SimpleListPolymorphic[Elem] {
    override def head = hd
    override def tail = tl
    override def isEmpty = false

    override def length: Int = 1 + tail.length

    override def map[Result](f: Elem => Result): SimpleListPolymorphic[Result] =
      f(head) :: tail.map(f)

    override def count(condition: Elem => Boolean): Int =
      if (condition(head))
        1 + tail.count(condition)
      else
        tail.count(condition)

    override def foldRight[Result](initial: Result,
                                   op: (Elem, Result) => Result)
                 : Result =
      op(head, tail.foldRight(initial, op))
  }

  /*
   * The API of SimpleList
   */
  def head: Elem
  def tail: SimpleListPolymorphic[Elem]
  def isEmpty: Boolean

  def length: Int
  def map[Result](f: Elem => Result): SimpleListPolymorphic[Result]
  def count(condition: Elem => Boolean): Int
  def foldRight[Result](initial: Result, op: (Elem, Result) => Result): Result

  
  /*
   * The :: method (pronounced "cons" as an abbreviation of "construct") constructs 
   * a new list by adding `newHead` to `this`. The result is a list with `newHead`
   * as its head and `this` as its tail. Example:
   * val list: SimpleListPolymorphic[Elem] == h :: t  with h:Elem and t: SimpleListPolymorphic[Elem] implies that
   * list.head == h && list.tail == t
   * 
   * The method :: associates to the right which also implies that it has the object 
   * it is invoked on to the right of the method name (in Scala, this is true for all
   * methods whose name ends with a :). Further, :: is usually used in infix notation, 
   * like this:
   * 1 :: 2 :: 3 :: EmptyPolymorphic == (1::(2::(3::EmptyPolymorphic))) == EmptyPolymorphic.::(3).::(2).::(1)
   * The alternative and more familiar syntax List(1, 2, 3) known from Scala's standard
   * List class is an invocation of the List companion object's apply method (the 
   * SimpleListPolymorphic object does not (yet) have an apply method).
   * 
   */
  def ::[NewElem >: Elem] (newHead: NewElem): SimpleListPolymorphic[NewElem] = new ListNode(newHead, this)
         /*
          * The additional type parameter NewElem (with Elem as its lower bound) is needed  
          * because Elem is covariant. The effect is that if you :: an NewElem object to a 
          * List[Elem], the resulting list will be of type List[NewElem]. E.g. adding a Person
          * to a SimpleListPolymorphic[Student] is possible and produces a SimpleListPolymorphic[Person].
          * 
          * You can safely ignore this if you only ever store Elem's in a SimpleListPolymorphic[Elem].
          */
  
  
  override def toString = "[" + elementsToString + "]"

  private def elementsToString: String = {
    if (isEmpty)
      ""
    else
       if (!tail.isEmpty)
         head.toString + ", " + tail.elementsToString
       else
         head.toString
  }
}

/**
 * The empty SimpleListPolymorphic. There is only one empty list, therefore this is
 * not a class, but a singleton object (precisely, a named, singleton 
 * instance of an anonymous subclass of SimpleListPolymorphic).
 */
object EmptyPolymorphic extends SimpleListPolymorphic[Nothing] {
/* Type scala.Nothing is a subtype of all Scala types.
 * Since SimpleListPolymorphic[+Elem] is covariant in Elem, this makes 
 * SimpleListPolymorphic[Nothing] a subtype of SimpleListPolymorphic[Elem] for 
 * any Elem, which makes it usable as the tail of any type of SimpleList. 
 * This means that everywhere the element type is needed below (i.e. in the 
 * signatures of methods map, find and foldRight), Nothing has to be used.
 * 
 * Note that "object EmptyPolymorphic extends SimpleListPolymorphic[Elem]" would 
 * be illegal, since Elem would be undefined here.
*/
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def isEmpty = true

  override def length: Int = 0
  override def map[Result](f: Nothing => Result): SimpleListPolymorphic[Result] = EmptyPolymorphic
  override def count(condition: Nothing => Boolean): Int = 0
  override def foldRight[Result](initial: Result, op: (Nothing, Result) => Result): Result = initial
}

/**
 * The companion object. This holds code and data for SimpleList's that is 
 * not specific for a specific SimpleListPolymorphic. Currently, there is only a main
 * method to test the class.
 */

object SimpleListPolymorphic {
  /**
   *  Just a bit of testing code
   */
  def main(args: Array[String]): Unit = {
    val squareNumbers = 1 :: 4 :: 9 :: 16 :: EmptyPolymorphic
    println("Square numbers doubled: " + squareNumbers.map(_*2))
    println("Number of odd square numbers: " + squareNumbers.count(_ % 2 != 0))
    println("Sum of square numbers: " + squareNumbers.foldRight(0, (_: Int) + (_: Int)))
                  // type ascription would not be needed with curried method invocation
  }
}