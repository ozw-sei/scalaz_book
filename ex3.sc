import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]) = list match{
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](list:List[A], value:A) = list match{
    case Nil => Nil
    case Cons(x, xs) => Cons(value, xs)
  }

  def drop[A](l:List[A], n: Int) = {
    @tailrec
    def _drop(l:List[A], current:Int): List[A] = l match{
      case Nil => l
      case Cons(x, xs) => if(current != 0) _drop(xs, current-1) else xs
    }
    _drop(l, n-1)
  }

  def append[A](a1:List[A], a2:List[A]) = a1 match{
    case Nil => a2
    case Cons(x, xs) => Cons(x, a2)
  }

  def init[A](l:List[A]) : List[A] = l match{
    case Nil => sys.error("error")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def dropWhile[A](l:List[A], f:A => Boolean) = {

    @tailrec
    def _dropWhile(l:List[A], f:A => Boolean):List[A] = {
      l match{
        case Cons(x, xs) => if(f(x)) _dropWhile(xs, f) else l
        case Nil => Nil
      }
    }
    _dropWhile(l, f)

  }
}

/**
  * ex3.1
  */
val ex31 = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // ここ通過するので3になる
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

/**
  * ex3.2
  * 先頭の要素を削除するtailを作成する
  */
val ex32 = List.tail(List(1,2,3))
/**
  * ex3.3
  * 先頭の要素を異なるものに置き換える
  */
val ex33 = List.setHead(List(1,2,3), 10)
/**
  * ex3.4
  * 先頭からn個の要素を削ったListを返す
  */
val ex34 = List.drop(List(1,2,3,4,5,6,7,8,9,10), 5)
/**
  * ex3.5
  * dropWhileの実装
  */
val ex35 = List.dropWhile(List(2,4,6,8,10, 1,3,5), (n:Int) => n % 2 == 0)

val ex36 = List.init(List(1,2,3,4,5))

