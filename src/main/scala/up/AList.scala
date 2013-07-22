package up


sealed trait AList[+M[_]] {
  type Fun[R]
  def apply0[R, N[X] >: M[X] ](f: N[Fun[R]])(implicit app: Apply[N]): N[R]
}

final case class ACons[H, T <: AList[M], +M[_]](head: M[H], tail: T) extends AList[M] {
  def @: [N[X] >: M[X], G](g: N[G]) = ACons(g, this: ACons[H, T, N])

  type Fun[R] = H => tail.Fun[R]
  override def apply0[R, N[X] >: M[X] ](f: N[H => tail.Fun[R]] )(implicit app: Apply[N]): N[R] =
    tail.apply0( app(f, head) )

  def apply[R, N[X] >: M[X] ](f: H => tail.Fun[R])(implicit p: Pure[N], app: Apply[N]): N[R] =
    apply0( p.pure(f) )(app)
}

sealed class ANil extends AList[Nothing] {
  def @: [M[_], H](h: M[H]) = ACons(h, this)
  type Fun[R] = R
  def apply0[R, N[X] >: Nothing ](n: N[R])(implicit app: Apply[N]): N[R] = n
}
object ANil extends ANil

// stubs for scalaz Apply, Pure
trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}
trait Pure[P[_]] {
  def pure[A](a: => A): P[A]
}
object Pure {
  implicit def OptionPure: Pure[Option] = new Pure[Option] {
    def pure[A](a: => A) = Some(a)
  }
}
object Apply {
  implicit def OptionApply: Apply[Option] = new Apply[Option] {
    def apply[A,B](f: Option[A => B], a: Option[A]): Option[B] =
      (f,a) match { case (Some(f), Some(a)) => Some(f(a)); case _ => None }
    }
}
