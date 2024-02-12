package dbg.schema

final case class Subtype[A, B](Underlying: Schema[B]) extends Subtype.Of[A] {
  final type Underlying = B
}
object Subtype {
  sealed trait Of[A] {
    type Underlying
    val Underlying: Schema[Underlying]

    final def cast(value: A): Underlying = value.asInstanceOf[Underlying]
  }
}
