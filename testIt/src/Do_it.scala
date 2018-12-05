
object Do_it {
  def main(args: Array[String]): Unit = {
    println("henlo hooman...")
    val x = { print("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    val result = z + y + x + z + y + x
  }
}
