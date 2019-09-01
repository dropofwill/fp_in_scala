import cats.kernel.Semigroup

import scala.annotation.tailrec

sealed trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]

object LinkedList {
  @tailrec
  def foldLeft[A,B](list: LinkedList[A], acc: B)(f: (B, A) => B): B = list match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)
  }

  def foldRightUnsafe[A, B](list: LinkedList[A], acc: B)(f: (A, B) => B): B = list match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }

  def foldRight[A, B](list: LinkedList[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(list), acc)((b,a) => f(a,b))

  def reverse[A](list: LinkedList[A]): LinkedList[A] =
    foldLeft(list, Nil:LinkedList[A])((xs: LinkedList[A], x: A) => Cons(x, xs))

  def len[A](list: LinkedList[A]): Long =
    foldLeft(list, 0)((acc, _) => acc + 1)

  def sum[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N =
    foldLeft(nums, numeric.zero)(numeric.plus)

  def product[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N =
    foldLeft(nums, numeric.one)(numeric.times)

  def sumCase[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N = nums match {
    case Nil => numeric.zero
    case Cons(x, xs) => numeric.plus(x, sum(xs))
  }

  def productCase[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N = nums match {
    case Nil => numeric.one
    case Cons(x, xs) => numeric.times(x, product(xs))
  }

  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](list: LinkedList[A], f: A => Boolean): LinkedList[A] =
    list match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f.apply(x)) dropWhile(xs, f)
        else Cons(x, xs)
      }
    }

  def drop[A](list: LinkedList[A], n: Int): LinkedList[A] =
    if (n == 0)
      list
    else
      list match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def tail[A](list: LinkedList[A]): LinkedList[A] = drop(list, 1)

  def setHead[A](a: A, list: LinkedList[A]): LinkedList[A] = list match {
    case Nil => Nil
    case Cons(_, x) => Cons(a, x)
  }

  def init[A](list: LinkedList[A]): LinkedList[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def append[A](list1: LinkedList[A], list2: LinkedList[A]): LinkedList[A] =
    foldRight(list1, list2)(Cons(_, _))

  def appendCase[A](list1: LinkedList[A], list2: LinkedList[A]): LinkedList[A] =
    list1 match {
      case Nil => list2
      case Cons(x, xs) => Cons(x, appendCase(xs, list2))
    }

  def flatten[A](list: LinkedList[LinkedList[A]]): LinkedList[A] =
    foldRight(list, Nil:LinkedList[A])(append)

  def map[A,B](list: LinkedList[A])(f: A => B): LinkedList[B] =
    foldRight(list, Nil:LinkedList[B])((a, xs) => Cons(f(a), xs))

  def flatMap[A,B](list: LinkedList[A])(f: A => LinkedList[B]): LinkedList[B] =
    foldRight(list, Nil:LinkedList[B])((a, xs) => append(f(a), xs))

  def where[A](list: LinkedList[A])(f: A => Boolean): LinkedList[A] =
    foldRight(list, Nil:LinkedList[A])((a, xs) => f(a) match {
      case true => Cons(a, xs)
      case false => xs
    })

  def zipWith[A](list1: LinkedList[A], list2: LinkedList[A])
                (implicit s: Semigroup[A]): LinkedList[A] = {
    def zipWithRec(
      l1: LinkedList[A], l2: LinkedList[A], acc: LinkedList[A]): LinkedList[A] =
      l1 match {
        case Nil => l2 match {
          case Nil => acc
          case Cons(_, _) => append(acc, l2)
        }
        case Cons(x1, xs1) => l2 match {
          case Nil => append(acc, l1)
          case Cons(x2, xs2) => zipWithRec(
            xs1, xs2, append(acc, LinkedList(s.combine(x1, x2))))
        }
      }
    zipWithRec(list1, list2, Nil)
  }

  // TODO come back to this when I understand typeclasses better
  // Would be nice to use Numeric
  def additionSemigroup: Semigroup[Int] = (x: Int, y: Int) => x + y
  def zipWithSum[Int](list1: LinkedList[Int], list2: LinkedList[Int])
                     (implicit semigroup: Semigroup[Int]): LinkedList[Int] = {
    zipWith(list1, list2)
  }
}

