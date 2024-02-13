package dbg.visitors

import dbg.schema.*

trait TraverseSchema {
  type Context
  type Out[A]

  def onSchema[A](schema: Schema[A])(using Context): Out[A] = schema match {
    case Schema.Primitive(name, primitive)         => onPrimitive(name, primitive)
    case Schema.Singleton(name, value)             => onSingleton(name, value)
    case Schema.Product(name, fields, construct)   => onProduct(name, fields, construct)
    case Schema.SumType(name, elements, construct) => onSumType(name, elements, construct)
    case Schema.Invariant(name, to, from, hint)    => onInvariant(name, to, from, hint)
  }

  // primitives

  def onPrimitive[A](name: TypeName[A], primitive: Primitive[A])(using Context): Out[A] = primitive match {
    case Primitive.Nothing => onNothing.asInstanceOf[Out[A]]
    case Primitive.Null    => onNull.asInstanceOf[Out[A]]
    case Primitive.Unit    => onUnit.asInstanceOf[Out[A]]
    case Primitive.Byte    => onByte.asInstanceOf[Out[A]]
    case Primitive.Boolean => onBoolean.asInstanceOf[Out[A]]
    case Primitive.Short   => onShort.asInstanceOf[Out[A]]
    case Primitive.Int     => onInt.asInstanceOf[Out[A]]
    case Primitive.Long    => onLong.asInstanceOf[Out[A]]
    case Primitive.Float   => onFloat.asInstanceOf[Out[A]]
    case Primitive.Double  => onDouble.asInstanceOf[Out[A]]
    case Primitive.Char    => onChar.asInstanceOf[Out[A]]
    case Primitive.String  => onString.asInstanceOf[Out[A]]
  }
  def onNothing(using Context): Out[Nothing]
  def onNull(using Context): Out[Null]
  def onUnit(using Context): Out[Unit]
  def onByte(using Context): Out[Byte]
  def onBoolean(using Context): Out[Boolean]
  def onShort(using Context): Out[Short]
  def onInt(using Context): Out[Int]
  def onLong(using Context): Out[Long]
  def onFloat(using Context): Out[Float]
  def onDouble(using Context): Out[Double]
  def onChar(using Context): Out[Char]
  def onString(using Context): Out[String]

  // singletons

  def onSingleton[A](name: TypeName[A], value: A)(using Context): Out[A]

  // products

  def onProduct[A](name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A)(using Context): Out[A]

  // sum types

  def onSumType[A](name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int)(using Context): Out[A]

  // invariants

  def onInvariant[A, B](name: TypeName[A], to: A => B, from: B => A, hint: MappingType[A, B])(using Context): Out[A]
}
