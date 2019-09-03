import scala.annotation.tailrec

sealed abstract class Tree[+A]
case object Term extends Tree
case class Node[A](left: Tree[A], right: Tree[A], value: A) extends Tree[A]

object Tree {
  def fold[A,B](tree: Tree[A], acc: B)(f: (B, A, B) => B): B = tree match {
    case Term => acc
    case Node(l, r, v) => f(
      fold(l, acc)(f),
      v,
      fold(r, acc)(f))
  }

  def size[A](tree: Tree[A]): Int =
    fold(tree, 0)((l, _, r) => l + r + 1)

  def max[N](tree: Tree[N])(implicit numeric: Numeric[N]): N =
    fold(tree, numeric.zero)((l, v, r) =>
      numeric.max(numeric.max(l, v), numeric.max(r, v)))

  def sum[N](tree: Tree[N])(implicit numeric: Numeric[N]): N =
    fold(tree, numeric.zero)((l, v, r) =>
      numeric.plus(numeric.plus(l, r), v))

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree, Term:Tree[B])((l, v, r) => Node(l, r, f(v)))
}
