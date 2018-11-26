/**
 * @author peine
 */
object Factorizer extends App {
  
  private def isFactor(factor: Int, number: Int) = number % factor == 0
  
  def factorize(number: Int): List[Int] =
    (2 to math.sqrt(number).toInt).foldLeft(List(): List[Int])(
      (factorsSoFar, n) => 
        if (isFactor(n, number))
          n :: (number / n) :: factorsSoFar
       else
         factorsSoFar)
         
  println("Factors of 237 = " + factorize(237))
}