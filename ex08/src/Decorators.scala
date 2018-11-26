/**
 * @author peine
 */
    
object Decorators extends App {

  type Logger = String => Unit

  type Decorator = Logger => Logger

  val uppercase: Decorator = 
        logger =>
          (msg: String) => logger(msg.toUpperCase) 
          
  val info: Decorator = 
        logger =>
          (msg: String) => logger("info: " + msg) 
    
  val prefixUppercase = info(uppercase(println(_)))
  val uppercasePrefix = uppercase(info(println(_)))
  prefixUppercase("This is Scala, prefixed and then uppercased")
  uppercasePrefix("This is Scala, uppercased and then prefixed")
}