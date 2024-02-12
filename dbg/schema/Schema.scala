package dbg.schema

import dbg.schema.Primitive as Primitives
import scala.collection.Factory
import scala.compiletime.*
import scala.deriving.Mirror
import scala.util.chaining.*

enum Schema[A] {
  case Primitive(name: TypeName[A], primitive: Primitives[A])
  case Singleton[A, B >: A](name: TypeName[A], value: A) extends Schema[A]
  case Product(name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A)
  case SumType(name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int)
  case Invariant[A, B](name: TypeName[A], to: A => B, from: B => A, hint: MappingType[A, B]) extends Schema[A]

  val name: TypeName[A]

  def narrow[B >: A]: Schema[B] = this.asInstanceOf[Schema[B]]
}
object Schema extends SchemaInstances0 {
  inline def apply[A](using A: Schema[A]): Schema[A] = A

  def singleton[A: TypeName](value: A): Schema[A] =
    Singleton(TypeName[A], value)

  def product[A: TypeName](fields: IArray[Field.Of[A]])(construct: IArray[Any] => A): Schema[A] =
    Product(TypeName[A], fields, construct)
  def product[A: TypeName](fields: Field.Of[A]*)(construct: IArray[Any] => A): Schema[A] =
    product(IArray.from(fields))(construct)

  def sumType[A: TypeName](elements: IArray[Subtype.Of[A]])(toOrdinal: A => Int): Schema[A] =
    SumType(TypeName[A], elements, toOrdinal)
  def sumType[A: TypeName](elements: Subtype.Of[A]*)(toOrdinal: Any => Int): Schema[A] =
    sumType(IArray.from(elements))(toOrdinal)

  def seqLike[A: Schema, CC <: IterableOnce[A]: TypeName](using fac: Factory[A, CC]): Schema[CC] =
    Invariant[CC, IterableOnce[A]](TypeName[CC], cc => cc, fac.fromSpecific, MappingType.SeqLike(Schema[A]))

  def mapLike[K: Schema, V: Schema, CC <: IterableOnce[(K, V)]: TypeName](using fac: Factory[(K, V), CC]): Schema[CC] =
    Invariant[CC, IterableOnce[(K, V)]](
      TypeName[CC],
      cc => cc,
      fac.fromSpecific,
      MappingType.MapLike(Schema[K], Schema[V])
    )

  def stringLike[A: TypeName](to: A => String)(from: String => A): Schema[A] =
    Invariant[A, String](TypeName[A], to, from, MappingType.NewType(Schema[String]))

  def secured[A: TypeName]: Schema[A] =
    Invariant[A, Nothing](TypeName[A], _ => ???, _ => ???, MappingType.Erased("secured"))
}

private[schema] trait SchemaInstances0 extends SchemaInstances1 { this: Schema.type =>

  // primitives
  given Schema[Nothing] = Primitive(TypeName.derived[Nothing], Primitives.Nothing)
  given Schema[Null]    = Primitive(TypeName.derived[Null], Primitives.Null)
  given Schema[Unit]    = Primitive(TypeName.derived[Unit], Primitives.Unit)
  given Schema[Byte]    = Primitive(TypeName.derived[Byte], Primitives.Byte)
  given Schema[Boolean] = Primitive(TypeName.derived[Boolean], Primitives.Boolean)
  given Schema[Short]   = Primitive(TypeName.derived[Short], Primitives.Short)
  given Schema[Int]     = Primitive(TypeName.derived[Int], Primitives.Int)
  given Schema[Long]    = Primitive(TypeName.derived[Long], Primitives.Long)
  given Schema[Float]   = Primitive(TypeName.derived[Float], Primitives.Float)
  given Schema[Double]  = Primitive(TypeName.derived[Double], Primitives.Double)
  given Schema[Char]    = Primitive(TypeName.derived[Char], Primitives.Char)
  given Schema[String]  = Primitive(TypeName.derived[String], Primitives.String)

  // collections

  given [A: scala.reflect.ClassTag: Schema]: Schema[Array[A]] =
    Invariant[Array[A], IterableOnce[A]](
      TypeName(s"scala.Array[${Schema[A].name}]"),
      _.toSeq,
      _.iterator.toArray[A],
      MappingType.SeqLike(Schema[A])
    )
  given [A: Schema, CC <: IterableOnce[A]: TypeName](using Factory[A, CC]): Schema[CC] =
    seqLike[A, CC]
  given [K: Schema, V: Schema, CC <: IterableOnce[(K, V)]: TypeName](using Factory[(K, V), CC]): Schema[CC] =
    mapLike[K, V, CC]

  given Schema[BigDecimal] = stringLike[BigDecimal](_.toString())(BigDecimal(_))
  given Schema[BigInt]     = stringLike[BigInt](_.toString())(BigInt(_))
}

private[schema] trait SchemaInstances1 { this: Schema.type =>

  inline given derived[A: Mirror.Of]: Schema[A] = inline summon[Mirror.Of[A]] match {
    case v: Mirror.Singleton =>
      singleton[A](v.asInstanceOf[A])
    case v: Mirror.SingletonProxy =>
      singleton[A](v.value.asInstanceOf[A])
    case p: Mirror.ProductOf[A] =>
      // TODO: add @secure annotation
      val name         = summonInline[ValueOf[p.MirroredLabel]].value
      var currentIndex = 0
      val fields =
        summonAll[Tuple.Zip[Tuple.Map[p.MirroredElemLabels, ValueOf], Tuple.Map[p.MirroredElemTypes, Schema]]]
          .map[[t] =>> Field.Of[A]] {
            [t] =>
              (pair: t) =>
                val index = currentIndex
                currentIndex += 1
                type Arbitrary
                val (label: ValueOf[String], schema: Schema[Arbitrary]) =
                  pair.asInstanceOf[(ValueOf[String], Schema[Arbitrary])]
                Field[A, Arbitrary](
                  schema,
                  label.value,
                  (a: A) => a.asInstanceOf[scala.Product].productElement(index).asInstanceOf[Arbitrary]
              )
          }
          .pipe { value =>
            value.toIArray.asInstanceOf[IArray[Field.Of[A]]]
          }
      val labels = fields.map(_.label)
      product(fields)(ArrayAsProduct(name, labels, _).pipe(p.fromProduct))
    case s: Mirror.SumOf[A] =>
      // TODO: add @secure annotation
      val name         = summonInline[ValueOf[s.MirroredLabel]].value
      var currentIndex = 0
      val subtypes = summonAll[Tuple.Map[s.MirroredElemTypes, Schema]]
        .map[[t] =>> Subtype.Of[A]] {
          [t] =>
            (pair: t) =>
              val index = currentIndex
              currentIndex += 1
              type Arbitrary
              val schema = pair.asInstanceOf[Schema[Arbitrary]]
              Subtype[A, Arbitrary](schema)
        }
        .pipe { value =>
          value.toIArray.asInstanceOf[IArray[Subtype.Of[A]]]
        }
      sumType(subtypes)(s.ordinal(_))
  }

  final private class ArrayAsProduct(name: String, labels: IArray[String], arr: IArray[Any]) extends scala.Product {
    self =>
    def productArity: Int              = arr.size
    def productElement(n: Int): Any    = arr(n)
    override def productPrefix: String = name
    override def productElementName(n: Int) =
      if n >= 0 && n < productArity then labels.applyOrElse(n, _ => "")
      else throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max ${arr.size - 1})")
    def canEqual(that: Any): Boolean = that match {
      case p1: scala.Product => productIterator.sameElements(p1.productIterator)
      case _                 => false
    }
  }
}
