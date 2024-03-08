package dbg.visitors

import dbg.schema.*

trait TraverseSchema {
  type Out[A]

  def onSchema[A](schema: Schema[A]): Out[A] = schema match {
    case Schema.Primitive(name, primitive)         => onPrimitive(name, primitive)
    case Schema.Singleton(name, value)             => onSingleton(name, value)
    case Schema.Product(name, fields, construct)   => onProduct(name, fields, construct)
    case Schema.SumType(name, elements, toOrdinal) => onSumType(name, elements, toOrdinal)
    case Schema.Invariant(name, to, from, hint)    => onInvariant(name, to, from, hint)
  }

  // primitives

  def onPrimitive[A](name: TypeName[A], primitive: Primitive[A]): Out[A] = primitive match {
    case Primitive.Nothing => onNothing
    case Primitive.Null    => onNull
    case Primitive.Unit    => onUnit
    case Primitive.Byte    => onByte
    case Primitive.Boolean => onBoolean
    case Primitive.Short   => onShort
    case Primitive.Int     => onInt
    case Primitive.Long    => onLong
    case Primitive.Float   => onFloat
    case Primitive.Double  => onDouble
    case Primitive.Char    => onChar
    case Primitive.String  => onString
  }
  def onNothing: Out[Nothing]
  def onNull: Out[Null]
  def onUnit: Out[Unit]
  def onByte: Out[Byte]
  def onBoolean: Out[Boolean]
  def onShort: Out[Short]
  def onInt: Out[Int]
  def onLong: Out[Long]
  def onFloat: Out[Float]
  def onDouble: Out[Double]
  def onChar: Out[Char]
  def onString: Out[String]

  // singletons

  def onSingleton[A](name: TypeName[A], value: A): Out[A]

  // products

  def onProduct[A](name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A): Out[A]

  // sum types

  def onSumType[A](name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int): Out[A]

  // invariants

  def onInvariant[A, B](name: TypeName[A], to: A => B, from: B => A, hint: MappingType[A, B]): Out[A] = hint match {
    case MappingType.NewType(implementation)   => onNewType(name, to, from, implementation)
    case seqLike: MappingType.SeqLike[a, c]    => onSeqLike[a, c](name, to, from, seqLike.item)
    case mapLike: MappingType.MapLike[a, k, v] => onMapLike[a, k, v](name, to, from, mapLike.key, mapLike.value)
    case MappingType.Erased(cause)             => onErased(name, to, from, cause)
  }
  def onNewType[A, B](name: TypeName[A], to: A => B, from: B => A, implementation: Schema[B]): Out[A]
  def onSeqLike[A, B](name: TypeName[A], to: A => IterableOnce[B], from: IterableOnce[B] => A, item: Schema[B]): Out[A]
  def onMapLike[A, K, V](
      name: TypeName[A],
      to: A => IterableOnce[(K, V)],
      from: IterableOnce[(K, V)] => A,
      key: Schema[K],
      value: Schema[V]
  ): Out[A]
  def onErased[A, B](name: TypeName[A], to: A => B, from: B => A, cause: String): Out[A]
}
