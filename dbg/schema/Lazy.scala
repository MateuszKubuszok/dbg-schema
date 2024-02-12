package dbg.schema

sealed trait Lazy[A] {
  lazy val value: A
}
object Lazy {
  def apply[A](thunk: => A): Lazy[A] = new {
    lazy val value: A = thunk
  }
}
