object ex10_2 {
  def one = 1
/*
  In the last line of the following Scala REPL session, the programmer intended to make fun an alias for
  one. Explain the resulting type error; its cause involves the difference between functions and methods,
  e.g. regarding the time of their evaluation. What should the programmer write instead in the right
  hand side of the definition of fun?

  one _
*/


  def main(args: Array[String]): Unit = {
    val fun: ()=>Int = one _
    print(fun.apply())

    /*
      Error:(5, 24) type mismatch;
      found   : Int
      required: () => Int
      val fun: ()=>Int = one
   */
  }
}
