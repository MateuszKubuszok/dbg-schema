package dbg.schema

import dbg.schema.Primitive as Primitives
import scala.collection.Factory
import scala.compiletime.*
import scala.deriving.Mirror
import scala.util.chaining.*
import scala.annotation.targetName
import scala.collection.View.Single

enum Schema[A] {
  case Primitive(name: TypeName[A], primitive: Primitives[A])
  case Singleton(name: TypeName[A], value: A)
  case Product(name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A) // TODO: use Lazy
  case SumType(name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int)     // TODO: use Lazy
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

  def erased[A: TypeName](cause: String): Schema[A] =
    Invariant[A, Nothing](TypeName[A], _ => ???, _ => ???, MappingType.Erased(cause))
  def secured[A: TypeName]: Schema[A] =
    erased[A]("secured")
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
      TypeName(s"scala.Array[${Schema[A].name.fullName}]"),
      _.toSeq,
      _.iterator.toArray[A],
      MappingType.SeqLike(Schema[A])
    )
  given [A: scala.reflect.ClassTag: Schema]: Schema[IArray[A]] =
    Invariant[IArray[A], IterableOnce[A]](
      TypeName(s"scala.IArray[${Schema[A].name.fullName}]"),
      _.toSeq,
      _.iterator.pipe(IArray.from),
      MappingType.SeqLike(Schema[A])
    )
  given [A: Schema, CC <: IterableOnce[A]: TypeName](using Factory[A, CC]): Schema[CC] =
    seqLike[A, CC]
  given [K: Schema, V: Schema, CC <: IterableOnce[(K, V)]: TypeName](using Factory[(K, V), CC]): Schema[CC] =
    mapLike[K, V, CC]

  // parsable from String
  given Schema[BigDecimal] = stringLike[BigDecimal](_.toString())(BigDecimal(_))
  given Schema[BigInt]     = stringLike[BigInt](_.toString())(BigInt(_))
  // TODO: java.time.{Duration, Instant}
  // TODO: java.util.UUID

  // exceptions
  given Schema[StackTraceElement] = product(
    Field[StackTraceElement, String](Schema[String], "classLoaderName", _.getClassLoaderName()),
    Field[StackTraceElement, String](Schema[String], "moduleName", _.getMethodName()),
    Field[StackTraceElement, String](Schema[String], "moduleVersion", _.getModuleVersion()),
    Field[StackTraceElement, String](Schema[String], "declaringClass", _.getClassName()),
    Field[StackTraceElement, String](Schema[String], "methodName", _.getMethodName()),
    Field[StackTraceElement, String](Schema[String], "fileName", _.getFileName()),
    Field[StackTraceElement, Int](Schema[Int], "lineNumber", _.getLineNumber())
  )(args =>
    new StackTraceElement(
      args(0).asInstanceOf[String], // classLoaderName
      args(1).asInstanceOf[String], // moduleName
      args(2).asInstanceOf[String], // moduleVersion
      args(3).asInstanceOf[String], // declaringClass
      args(4).asInstanceOf[String], // methodName
      args(5).asInstanceOf[String], // fileName
      args(6).asInstanceOf[Int]     // lineNumber
    )
  )
  // TODO: generic approach?
  given Schema[Throwable] = product(
    Field[Throwable, String](Schema[String], "message", _.getMessage()),
    // TODO: introduce Lazy to fix this
    // Field[Throwable, Throwable](Schema[Throwable], "cause", _.getCause()),
    Field[Throwable, Array[StackTraceElement]](Schema[Array[StackTraceElement]], "stackTrace", _.getStackTrace())
  )(args =>
    new Throwable(args(0).asInstanceOf[String], args(1).asInstanceOf[Throwable])
      .tap(_.setStackTrace(args(2).asInstanceOf[Array[StackTraceElement]]))
  )

  // erased types
  given [A, B](using TypeName[A => B]): Schema[A => B] = erased[A => B]("function")

  // Schema components
  given [A]: Schema[TypeName[A]] = {
    given TypeName[TypeName[A]] = typeName"dbg.schema.TypeName[...]".of[TypeName[A]]
    derived[TypeName[A]]
  }
  given [A]: Schema[Primitives[A]] = derived[Primitives[A]]
  given [A]: Schema[Lazy[A]] = {
    given TypeName[Lazy[A]] = typeName"dbg.schema.Lazy[...]".of[Lazy[A]]
    given Schema[A]         = erased("schema")
    product(Field[Lazy[A], A](Schema[A], "lazy", _.value))(args => Lazy(args(0).asInstanceOf[A]))
  }
  @targetName("given_Schema_Product_Of")
  given [A]: Schema[Field.Of[A]] = ??? // TODO
  @targetName("given_Schema_Subtype_Of")
  given [A]: Schema[Subtype.Of[A]]        = ??? // TODO
  given [A, B]: Schema[MappingType[A, B]] = ??? // TODO

  // Schema elements
  given [A]: Schema[Primitive[A]] = {
    given TypeName[Primitive[A]] = typeName"dbg.schema.Primitive[...]".of[Primitive[A]]
    derived[Primitive[A]]
  }
  given [A]: Schema[Singleton[A]] = {
    given TypeName[Singleton[A]] = typeName"dbg.schema.Singleton[...]".of[Singleton[A]]
    given Schema[A]              = erased("schema")
    derived[Singleton[A]]
  }
  given [A]: Schema[Product[A]] = {
    given TypeName[Product[A]] = typeName"dbg.schema.Product[...]".of[Product[A]]
    derived[Product[A]]
  }
  given [A: Schema]: Schema[SumType[A]] = {
    given TypeName[SumType[A]] = typeName"dbg.schema.SumType[...]".of[SumType[A]]
    derived[SumType[A]]
  }
  given [A, B]: Schema[Invariant[A, B]] = {
    given TypeName[Invariant[A, B]] = typeName"dbg.schema.Invariant[..., ...]".of[Invariant[A, B]]
    derived[Invariant[A, B]]
  }
  given [A]: Schema[Schema[A]] = {
    given TypeName[Schema[A]] = typeName"dbg.schema.Schema[...]".of[Schema[A]]
    derived[Schema[A]]
  }
}

private[schema] trait SchemaInstances1 { this: Schema.type =>

  inline given derived[A: Mirror.Of]: Schema[A] = inline summon[Mirror.Of[A]] match {
    case v: Mirror.Singleton =>
      singleton[A](v.asInstanceOf[A])
    case v: Mirror.SingletonProxy =>
      singleton[A](v.value.asInstanceOf[A])
    case p: Mirror.ProductOf[A] =>
      // TODO: add @secure annotation
      val name = summonInline[ValueOf[p.MirroredLabel]].value
      val labels =
        summonAll[Tuple.Map[p.MirroredElemLabels, ValueOf]].toIArray.asInstanceOf[IArray[ValueOf[String]]].map(_.value)
      val schemas = summonAll[Tuple.Map[p.MirroredElemTypes, Schema]].toIArray.asInstanceOf[IArray[Schema[?]]]
      val fields = labels.zip(schemas).zipWithIndex.map { case ((label, schema), index) =>
        type Arbitrary
        Field[A, Arbitrary](
          schema.asInstanceOf[Schema[Arbitrary]],
          label,
          (a: A) => a.asInstanceOf[scala.Product].productElement(index).asInstanceOf[Arbitrary]
        ): Field.Of[A]
      }
      product(fields)(ArrayAsProduct(name, labels, _).pipe(p.fromProduct))
    case s: Mirror.SumOf[A] =>
      // TODO: add @secure annotation
      val name = summonInline[ValueOf[s.MirroredLabel]].value
      val subtypes = summonAll[Tuple.Map[s.MirroredElemTypes, Schema]].toIArray
        .asInstanceOf[IArray[Schema[?]]]
        .zipWithIndex
        .map { case (schema, index) =>
          type Arbitrary
          Subtype[A, Arbitrary](schema.asInstanceOf[Schema[Arbitrary]]): Subtype.Of[A]
        }
      sumType(subtypes)(s.ordinal(_))
  }

  final class ArrayAsProduct(name: String, labels: IArray[String], arr: IArray[Any]) extends scala.Product {
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
