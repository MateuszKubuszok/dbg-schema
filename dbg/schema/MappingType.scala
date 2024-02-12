package dbg.schema

enum MappingType[A, B] {
  case NewType(implementation: Schema[B])
  case SeqLike[A, B](item: Schema[B])                     extends MappingType[A, IterableOnce[B]]
  case MapLike[A, K, V](key: Schema[K], value: Schema[V]) extends MappingType[A, IterableOnce[(K, V)]]
  case Erased(cause: String)
}
