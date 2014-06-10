package up


trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}

object ~> {
  type Id[X] = X
  trait Const[A] { type Apply[B] = A }
  implicit def idEq : Id ~> Id = new (Id ~> Id) { def apply[T](a: T): T = a }
}

trait Param[A[_], B[_]] {
  type T
  def in: A[T]
  def ret(out: B[T])
  def ret: B[T]
}

object Param {
  implicit def pToT[A[_], B[_]](p: Param[A,B] => Unit): A~>B = new (A ~> B) {
    def apply[s](a: A[s]): B[s] = {
      val v: Param[A,B] { type T = s} = new Param[A,B] { type T = s
        def in = a
        private var r: B[T] = _
        def ret(b: B[T]) {r = b}
        def ret: B[T] = r
      }
      p(v)
      v.ret
    }
  }
}
