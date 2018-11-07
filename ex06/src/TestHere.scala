class TestHere {
  def length(list: List[Int]): Int = {
    var x = list; var len = 0
    while(x.nonEmpty) {len = len + 1; x = x.tail}
    len
  }
  object TestHere {
    def main(args: Array[String]): Unit = {
      println("Hello, world!")
    }
  }

}
