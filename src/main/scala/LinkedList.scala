sealed trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]

object LinkedList {
  def sum[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N = nums match {
    case Nil => numeric.zero
    case Cons(x, xs) => numeric.plus(x, sum(xs))
  }

  def product[N](nums: LinkedList[N])(implicit numeric: Numeric[N]): N = nums match {
    case Nil => numeric.one
    case Cons(0, _) => numeric.zero
    case Cons(x, xs) => numeric.times(x, product(xs))
  }

  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](list: LinkedList[A], f: A => Boolean): LinkedList[A] =
    list match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f.apply(x))
          dropWhile(xs, f)
        else
          Cons(x, xs)
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

  def setHead[A](a: A, list: LinkedList[A]) = list match {
    case Nil => Nil
    case Cons(_, x) => Cons(a, x)
  }
}
