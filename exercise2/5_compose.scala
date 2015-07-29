object Exercise5{
  def compose[A, B, C](f: B=>C, g:A => B) = (a:A) => f(g(a))

  def main(args: Array[String]): Unit = {
    val f = compose[Int, Int, Int]((a:Int) => a +1, (b:Int) => b + 1)
    if(f(10) == 12) println("correct")
    else println("incorrect")
  }
}
