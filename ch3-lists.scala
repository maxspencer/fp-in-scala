sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, as2) => as2
  }

  // Exercise 3
  def drop[A](as: List[A], n: Int): List[A] = 
    if (n == 0) as
    else as match {
      case Nil => Nil
      case Cons(_, as2) => drop(as2, n - 1)
    }

  // Exercise 4
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(a, as2) =>
      if (f(a)) dropWhile(as2)(f)
      else Cons(a, dropWhile(as2)(f))
  }

  // Exercise 5
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(b, bs) => Cons(a, bs)
  }

  // Exercise 10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def go(acc: B, as2: List[A]): B = as2 match {
      case Nil => acc
      case Cons(a, as3) => go(f(acc, a), as3)
    }
    go(z, as)
  }

  // Exercise 11
  def foldLeftSum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def foldLeftProduct(ds: List[Double]): Double = foldLeft(ds, 0.0)(_ * _)

  def foldLeftLength[A](as: List[A]): Int = foldLeft(as, 0)((i, _) => i + 1)

  // Exercise 12

  // First solution using append is non-tail recursive and naive
  def append[A](as: List[A], a: A): List[A] = match as {
    case Nil => Cons(a, Nil)
    case Cons(a2, as2) => Cons(a2, append(as2, a))
  }
  
  def reverse[A](as: List[A]): List[A] = match as {
    case Nil => Nil
    case Cons(a, as2) => append(reverse(as2), a)
  }
  

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

}
