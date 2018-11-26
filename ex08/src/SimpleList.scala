
/**
 * A simple singly-linked immutable list, parameterized with the type of its elements.
 * An empty SimpleList is simply an object.
 * A non-empty SimpList is a ListNode; actually, since a ListNode references its neighbor, 
 * it is more like a chain of ListNodes: A typical recursive data structure. The element
 * contained in the ListNode is called the head of the list (i.e. its first element). 
 * The other ListNodes (i.e. the rest of the list's elements) collectively form another
 * SimpleList, called the tail of the original list.
 * 
 * In this version, the code of every method is completely contained in the respective
 * method of ListNode by means of an if (isEmpty) ... else ... . This is easier to 
 * understand since the whole code of a method is one place and object EmptyList can be
 * defined nearly without using the (as yet unexplained) type Nothing. Note, however,
 * that such if ... else based on dynamic types ("type switch") is not good object-oriented
 * style. This is why there is a second, more object-oriented but slightly more complicated
 * solution in file SimpleListPolymorphic.scala.
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
abstract class SimpleList[+Elem] {
          /*
           * Elem is a type parameter for the element type.
           * +Elem declares SimpleList[Elem] to be covariant with respect to Elem: 
           * This ensures that a List[Other] is a subtype of List[Elem] if Other 
           * is a subtype of Elem. For example, List[Student] is a subtype of List[Person] then.
           * Such a subtype relationship is type-safe if and only if the List is immutable.
           * 
           * You can safely ignore that "+" if you don't need the subtype property. Variance of 
           * type parameters will not be treated in this course (only variance of method 
           * parameter types will be treated in the context of the object-oriented paradigm)
           */
          
  /**
   * An internal helper class storing the head and the tail of a
   * non-empty SimpleList.
   */
  private class ListNode[Elem](hd: Elem, tl: SimpleList[Elem]) extends SimpleList[Elem] {
    override def head = hd
    override def tail = tl
    override def isEmpty = false
  }

  /*
   * The API of SimpleList
   */
  def head: Elem
  def tail: SimpleList[Elem]
  def isEmpty: Boolean

  
  /*
   * The :: method (pronounced "cons" as an abbreviation of "construct") constructs 
   * a new list by adding `newHead` to `this`. The result is a list with `newHead`
   * as its head and `this` as its tail. Example:
   * val list: SimpleList[Elem] == h :: t  with h:Elem and t: SimpleList[Elem] implies that
   * list.head == h && list.tail == t
   * 
   * The method :: associates to the right which also implies that it has the object 
   * it is invoked on to the right of the method name (in Scala, this is true for all
   * methods whose name ends with a :). Further, :: is usually used in infix notation, 
   * like this:
   * 1 :: 2 :: 3 :: Empty == (1::(2::(3::Empty))) == Empty.::(3).::(2).::(1)
   * The alternative and more familiar syntax List(1, 2, 3) known from Scala's standard
   * List class is an invocation of the List companion object's apply method (the 
   * SimpleList object does not (yet) have an apply method).
   * 
   */
  def ::[NewElem >: Elem] (newHead: NewElem): SimpleList[NewElem] = new ListNode(newHead, this)
         /*
          * The additional type parameter NewElem (with Elem as its lower bound) is needed  
          * because Elem is covariant. The effect is that if you :: an NewElem object to a 
          * List[Elem], the resulting list will be of type List[NewElem]. E.g. adding a Person
          * to a SimpleList[Student] is possible and produces a SimpleList[Person].
          * 
          * You can safely ignore this if you only ever store Elem's in a SimpleList[Elem].
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
  
  def length: Int = {
    if (isEmpty) 0
    else 1 + tail.length
  }
 
  def map[Result](f: Elem => Result): SimpleList[Result] =
    if (isEmpty)
      EmptyList
    else
      f(head) :: tail.map(f)
        
  def mapByFold[Result](f: Elem => Result): SimpleList[Result] =
    foldRight(EmptyList, (e: Elem, resultSoFar: SimpleList[Result]) => 
                            f(e) :: resultSoFar)  
                         // Note that using simply  f(_) :: _   as the function argument 
                         // would be an error since the type of _ could not be inferred
                         // because EmptyList does not have information about the element type
                          
  def count(condition: Elem => Boolean): Int =
    if (isEmpty)
      0
    else
      if (!condition(head))
         tail.count(condition)
      else
         1 + tail.count(condition)  
        
  def foldRight[Result](initial: Result, 
                        op: (Elem, Result) => Result): Result =
                          /**
                           * fold methods should use the curried method parameter syntax
                           * (i.e. foldRight(initial: Result)(op: ...))
                           * in order to facilitate type inference when calling that method.
                           */
    if (isEmpty)
      initial
    else
      op(head, tail.foldRight(initial, op))
}

/**
 * The empty SimpleList. There is only one empty list, therefore this is
 * not a class, but a singleton object (precisely, a named, singleton 
 * instance of an anonymous subclass of SimpleList). 
 */
object EmptyList extends SimpleList[Nothing] {
/* The type scala.Nothing is a subtype of all Scala types ("bottom type").
 * Since SimpleList[+Elem] is covariant in Elem, this makes SimpleList[Nothing]
 * a subtype of SimpleList[Elem] for any Elem, which makes it usable as the 
 * tail of any type of SimpleList. This means that everywhere the element
 * type is needed below (i.e. in the signatures of methods map, find and foldRight),
 * Nothing has to be used.
 * 
 * Note that "object EmptyList extends SimpleList[Elem]" would be  illegal, 
 * since Elem would be undefined here.
*/
      override def head = throw new NoSuchElementException
      override def tail = throw new NoSuchElementException
      override def isEmpty = true

}

/**
 * The companion object. This holds code and data for SimpleList's that is 
 * not specific for a specific SimpleList. Currently, there is only a main
 * method to test the class.
 */

object SimpleList {
  /**
   *  Just a bit of testing code
   */
  def main(args: Array[String]): Unit = {
    val squareNumbers = 1 :: 4 :: 9 :: 16 :: EmptyList
    println("Square numbers doubled: " + squareNumbers.map(_*2))
    println("Number of odd square numbers: " + squareNumbers.count(_ % 2 != 0))
    println("Sum of square numbers: " + squareNumbers.foldRight(0, (_: Int) + (_: Int)))
                  // type ascription would not be needed with curried method invocation
 }
}
