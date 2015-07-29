object Excercise3{
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a, b)

  def main(args: Array[String]): Unit = {
    val a = curry[Int, String, Int]((a:Int, b:String) => a + b.length)
    val b = a(10)
    if(b("test") == 14) println("correct")
    else println("incorrect")
  }
}
