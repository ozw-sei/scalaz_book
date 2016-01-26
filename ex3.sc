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

  def tail[A](list: List[A]):List[A] = list match{
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](list:List[A], value:A):List[A] = list match{
    case Nil => Nil
    case Cons(x, xs) => Cons(value, xs)
  }

  def drop[A](l:List[A], n: Int):List[A] = {
    @tailrec
    def _drop(l:List[A], current:Int): List[A] = l match{
      case Nil => l
      case Cons(x, xs) => if(current != 0) _drop(xs, current-1) else xs
    }
    _drop(l, n-1)
  }

  def append[A](a1:List[A], a2:List[A]):List[A] = a1 match{
    case Nil => a2
    case Cons(x, xs) => Cons(x, a2)
  }

  def init[A](l:List[A]) : List[A] = l match{
    case Nil => sys.error("error")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def dropWhile[A](l:List[A], f:A => Boolean):List[A] = {

    @tailrec
    def _dropWhile(l:List[A], f:A => Boolean):List[A] = {
      l match{
        case Cons(x, xs) => if(f(x)) _dropWhile(xs, f) else l
        case Nil => Nil
      }
    }
    _dropWhile(l, f)

  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def foldRight2[A, B](as:List[A], z:B)(f:(A, B) => B): B = {
    foldLeft(reverse(as), z)((b:B, a:A) => f(a, b))
  }

  def foldLeft[A, B](as:List[A], z:B)(f:(B, A) => B): B = {
    as match{
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
    }
  }

  def sum2(ints: List[Int]): Int =
    List.foldLeft(ints, 0)((left:Int, right:Int) => left + right)

  def length[A](as:List[A]):Int = foldRight(as, 0)((left, right) => right +1)

  def product2(ints:List[Double]):Double =
    List.foldLeft(ints, 1.0)((left:Double, right:Double) => left * right)

  def length2[A](ints:List[A]):Int = List.foldLeft(ints, 0)((left:Int, right:A) => left + 1)

  def reverse[A](arr:List[A]): List[A] =
    List.foldLeft(arr, Nil:List[A])((left:List[A], right:A) => Cons(right, left))
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
/**
  * ex3.6
  * 最後尾より前の要素を取り出し
  */
val ex36 = List.init(List(1,2,3,4,5))
/**
  * 3.7(答え見た）
  * 仮に例外値をキャッチして処理を切り替えることはできない。
  * なぜならばfoldRightの中でf()より前に処理を挟むことができないので例外をハンドルできる箇所がない
  */


/**
  * 3.8
  * foldRight(List(1,2,3), Nil)(Cons(_,_))
  * のような渡され方をした場合はCons(Nil, List(1,2,3))になるが、
  * foldRightでNilがSkipされるので結果的には変わらない
  */
val ex38 = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
/**
  * 3.9 foldRightを使用してlengthを作る
  * 楽勝
  */
val ex39 = List.length(List(1,2,3,4,5))
/**
  * 3.10
  * foldLeftを実装
  */
val ex310 = List.foldLeft(List(1,2,3,4,5), 100)((hoge, fuga) => hoge+fuga)
/**
  * 3.11
  * foldLeftを使用してsum/product/lengthを作る
  */
val ex311_length = List.length2(List(1,2,3,4,5)) // 5
val ex311_sum = List.sum2(List(1,2,3,4,5)) // 15
val ex311_product = List.product2(List(1.0, 2.0, 3.0)) // 6.0

/**
  * 3.12
  * Listを逆にする関数を作れ
  */
val ex312:List[Int] = List.reverse(List(1,2,3)) // List(3,2,1)


/**
  * 3.13
  * foldLeftを使用してfoldRightを実装
  */
val ex313 = List.foldRight2(List(10.0:Double, 4.0:Double), 2.0:Double)((left, right) => right - left)
