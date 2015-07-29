def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def go(n: Int, prev:A): Boolean = {
    if(as.length <= n) true
    else if(ordered(prev, as(n))) go(n+1, as(n))
    else false
  }
  go(1, as(0))
}
