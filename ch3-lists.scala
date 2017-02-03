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
  // First solution is not tail recursive
  def appendOne[A](as: List[A], a: A): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(a2, as2) => Cons(a2, appendOne(as2, a))
  }
  
  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(a, as2) => appendOne(reverse(as2), a)
  }

  // This solutions using foldLeft is tail recursive
  def foldReverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  // Exercise 13
  // Implement foldLeft in terms of foldRight
  def foldLeft2[A, B](as: List[A], b: B)(f: (B, A) => B): B = foldRight(reverse(as), b)((a, b) => f(b, a))

  // Exercise 14
  // Implement append in terms of foldLeft or foldRight
  def foldLeftAppend[A](as1: List[A], as2: List[A]): List[A] = foldLeft(reverse(as1), as2)((a,b) => Cons(b, a))
  def foldRightAppend[A](as1: List[A], as2: List[A]): List[A] = foldRight(as1, as2)(Cons(_, _))

  // Exercise 15
  // Write a function that concatenates a list of lists into a single list, time complexity should be linear
  // in total length of all the supplied lists
  def concat[A](ass: List[List[A]]): List[A] = foldRight(ass, Nil: List[A])(foldRightAppend)

  // Exercise 16
  // Write a function that takes a list of integers and adds one to each of
  // those integers. It should return a new list
  def incrementEach(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(int, ints2) => Cons(int + 1, incrementEach(ints2))
  }

  def foldRightIncrementEach(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((int, ints2) => Cons(int + 1, ints2))

  def foldLeftIncrementEach(ints: List[Int]): List[Int] =
    foldLeft(ints, Nil: List[Int])((ints2, int) => appendOne(ints2, int + 1))

  // Exercise 17
  // Write a function that turns each double in a list into a string
  def toStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, strings) => Cons(d.toString, strings))

  // Exercise 18
  // Write a function called map that generalises modifying each element of
  // a list, but preserves its structure
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  // Exercise 19
  // Implement a function filter which removes items from a list unless they
  // satisfy a given predicate
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, bs) => if (f(a)) Cons(a, bs) else bs)

  // Exercise 20
  // Implement flatMap which is like map, but where the function outputs a
  // list and the overall output is a flat list
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 21
  // Implement filter using flatMap
  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // Exercise 22
  // Implement a function that adds together corresponding integers
  def addInts(ints1: List[Int], ints2: List[Int]): List[Int] = ints1 match {
    case Nil => Nil
    case Cons(i1, ints3) => ints2 match {
      case Nil => Nil
      case Cons(i2, ints4) => Cons(i1 + i2, addInts(ints3, ints4))
    }
  }

  // Exercise 23
  // Generalise addInts
  def map2[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    as match {
      case Nil => Nil
      case Cons(ah, at) => bs match {
        case Nil => Nil
	case Cons(bh, bt) => Cons(f(ah, bh), map2(at, bt)(f))
      }
    }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

}
