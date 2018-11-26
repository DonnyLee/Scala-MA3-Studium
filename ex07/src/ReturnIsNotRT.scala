/**
 * @author peine
 */

object ReturnIsNotRT extends App {
  def atMost100(n:Int): Int = {
    val e  = return 100
    if (n < 100) n else {
      e
      // This is never reached  
      }
  }
  
  println(atMost100(50))   // Prints 100 after replacing return 100 by its vale (i.e. by e)
  println(atMost100(200))
}