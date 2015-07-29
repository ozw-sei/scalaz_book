object Exercise4{
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a:A, b:B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    val curried = uncurry[Int, String, Int]((a:Int) => (b:String) => a + b.length)
    val result = curried(10, "test")
    if(result == 14) println("correct")
    else println("incorrect")
  }

}
