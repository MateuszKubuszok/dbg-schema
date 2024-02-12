package dbg.schema

final case class Field[A, B](Underlying: Schema[B], label: String, extractor: A => B) extends Field.Of[A] {
  final type Underlying = B
  override def extract(value: A): Underlying = extractor(value)
}
object Field {
  sealed trait Of[A] {
    type Underlying
    val Underlying: Schema[Underlying]

    def extract(value: A): Underlying
    val label: String
  }
}
