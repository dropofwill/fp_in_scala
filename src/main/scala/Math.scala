import scala.annotation.tailrec

object Math {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @tailrec
    def factAcc(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else factAcc(n - 1, n * acc)

    factAcc(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def fibAcc(n: Int, x1: Int, x2: Int): Int =
      if (n == 0) x1
      else if (n == 1) x2
      else fibAcc(n - 1, x2, x1 + x2)

    fibAcc(n, 0, 1)
  }

  def isSorted[A](arr: Array[A])(implicit ordering: Ordering[A]): Boolean = {
    @tailrec
    def isSortedAcc(arr: Array[A], n: Int, acc: Boolean): Boolean = {
      if (n >= arr.length - 1) acc
      else isSortedAcc(
        arr,
        n + 1,
        ordering.lteq(arr.apply(n), arr.apply(n + 1)) && acc
      )
    }

    isSortedAcc(arr, 0, true)
  }
}
