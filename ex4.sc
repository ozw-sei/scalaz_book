import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match{
    case Some(b) => Some(f(b))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match{
    case Some(b) => b
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match{
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match{
    case Some(b) => Some(b)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match{
    case Some(a) if(f(a)) => Some(a)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
val ex1map = Some(2).map(_/2) // Some(1)
val ex1getOrElse = None.getOrElse(-1)
val ex1flatMap = Some(2).flatMap(_ => None)

val ex1orElse = None.orElse(Some(2))
val ex1filter = Some(2).filter(_%2 ==1)

object Option {
////  def failingFn(i: Int): Int = {
////    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
////    try {
////      val x = 42 + 5
////      x + y
////    }
////    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
////  }
////
////  def failingFn2(i: Int): Int = {
////    try {
////      val x = 42 + 5
////      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
////    }
////    catch { case e: Exception => 43 }
////  }
//
//  def mean(xs: Seq[Double]): Option[Double] =
//    if (xs.isEmpty) None
//    else Some(xs.sum / xs.length)
//  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")
//
//  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = sys.error("todo")
//
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")
//
//  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}