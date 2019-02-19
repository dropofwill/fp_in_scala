object Function {

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f.apply(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f.apply(a).apply(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f.apply(g.apply(a))
}
