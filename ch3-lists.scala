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

  // Exercise 6
  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(a, as2) => Cons(a, init(as2))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(a, as2) => f(a, foldRight(as2, z)(f))
  }

  def foldSum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def foldProduct(ds: List[Double]): Double = foldRight(ds, 1.0)(_ + _)

  // Exercise 8
  val ex8 = foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))

  // Exercise 9
  def foldLength[A](as: List[A]): Int = {
    def incrementRight[B](b: B, i: Int): Int = i + 1
    foldRight(as, 0)(incrementRight(_,_))
  }

  // Exercise 10
  //  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = 

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

}
