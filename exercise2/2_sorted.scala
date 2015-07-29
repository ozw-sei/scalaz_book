object Excercise2{


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int, prev:A): Boolean = {
      if(as.length <= n) true
      else if(ordered(prev, as(n))) go(n+1, as(n))
      else false
    }
    go(1, as(0))
  }

  def main(args: Array[String]): Unit = {
    val a = isSorted[Int](Array(1,2,3,4,5, 1), (left :Int, right:Int) => left < right)
    if(a) println("incorrect")

    val b = isSorted[Int](Array(1,2,3,4,5), (left :Int, right:Int) => left < right)
    if(!b) println("incorrect")

    println("correct")
  }

}
