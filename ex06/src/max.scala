

/**
 * @author peine
 */
object max {

  type IntFun = Int => Int // for readability
  // YOUR CODE FOR MAX HERE
  type g = Int
  type f = Int
  val max = (g:Int, f:Int) => if (g>f) g else f

  def main(args: Array[String]): Unit = {
     val tripler = (x: Int) => 3*x
     val square = (x: Int) => x * x
//     val maxTriplerSquare = max(tripler, square)
//     for (i <- 1 to 5)
//       println(maxTriplerSquare(i))
      println(max(6,5))

  } 
}