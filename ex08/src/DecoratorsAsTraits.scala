object DecoratorsAsTraits extends App {

  trait Logger {
    def log(m: String): Unit
  }
  
  trait Uppercase extends Logger {
    abstract override def log(m: String) = super.log(m.toUpperCase)
  }

  trait Info extends Logger {
    abstract override def log(m: String) = super.log("info: " + m)
  }
  
  class ConsoleLogger extends Logger {
    def log(m: String) = println(m)
  }

  val prefixUppercase = new ConsoleLogger with Uppercase with Info
  val uppercasePrefix = new ConsoleLogger with Info with Uppercase
  prefixUppercase.log("This is Scala, prefixed and then uppercased")
  uppercasePrefix.log("This is Scala, uppercased and then prefixed")
}