
abstract class MyStack[+Elem]{
  def top:Elem
  def isEmpty:Boolean = false

}
object EmptyStack extends MyStack[Nothing] {
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
  override def top = throw new NoSuchElementException
  override def isEmpty = true

}


object Do_it {
  def main(args: Array[String]): Unit = {
    println("henlo hooman...")

  }
}
