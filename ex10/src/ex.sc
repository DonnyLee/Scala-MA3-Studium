object ex10_1 {
  //Write a currying method
  val charAt = (s:String, n:Int) => s(n) //return the n'th character of a String s

  val curriedCharAt = curried(charAt)
  val hannoverAt = curriedCharAt("Hannover")
  /*
    val curriedCharAt = (s:String) => (n:Int) => s.charAt(n)
    val hannoverAt = curriedCharAt("Hannover")
  */
  def curried(fCharAt: (String, Int) => Char) = fCharAt(_)

  def main(args: Array[String]): Unit = {

    println(hannoverAt(4))  //prints the character o

  }
}