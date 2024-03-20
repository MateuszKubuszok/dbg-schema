package dbg.visitors

import dbg.schema.*

object ShowSchema extends TraverseSchema {
  final case class Context(
      indentation: String,
      lineSeparation: String,
      nesting: Int,
      builder: StringBuilder
  ) {
    def append(msg: String): Context = {
      builder.append(msg)
      this
    }

    def indent: Context = {
      var i = nesting
      while i > 0 do {
        builder.append(indentation)
        i -= 1
      }
      this
    }

    def ln: Context = append(lineSeparation)

    def nest: Context = copy(nesting = nesting + 1)
  }
  inline def ctx(using ctx: Context): Context = ctx

  type Out[A] = Context ?=> A => Unit

  def prettyPrint[A: Schema](
      value: A,
      indentation: String = "  ",
      lineSeparation: String = "\n",
      nesting: Int = 0
  ): String = {
    val builder = new StringBuilder
    onSchema(Schema[A])(using
      Context(
        indentation = indentation,
        lineSeparation = lineSeparation,
        nesting = nesting,
        builder = builder
      )
    )(value)
    builder.toString()
  }

  def onNothing: Out[Nothing] = _ => ctx.append("???")
  def onNull: Out[Null]       = _ => ctx.append("null")
  def onUnit: Out[Unit]       = _ => ctx.append("()")
  def onByte: Out[Byte]       = a => ctx.append(a.toString + ".toByte")
  def onBoolean: Out[Boolean] = a => ctx.append(a.toString)
  def onShort: Out[Short]     = a => ctx.append(a.toString + ".toShort")
  def onInt: Out[Int]         = a => ctx.append(a.toString)
  def onLong: Out[Long]       = a => ctx.append(a.toString + "L")
  def onFloat: Out[Float]     = a => ctx.append(a.toString + "f")
  def onDouble: Out[Double]   = a => ctx.append(a.toString)
  def onChar: Out[Char]       = a => ctx.append("'" + a.toString + "'")
  def onString: Out[String]   = a => ctx.append("\"" + a.toString + "\"")

  def onSingleton[A](name: TypeName[A], value: A): Out[A] = _ => ctx.append(name.fullName + ".type")

  def onProduct[A](name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A): Out[A] = a => {
    ctx.append(name.fullName).append("(")
    if fields.nonEmpty then {
      fields.zipWithIndex.foreach { case (field, i) =>
        if i > 0 then {
          ctx.append(",")
        }
        import field.{Underlying as Field, *}
        ctx.ln.nest.indent.append(label).append(" = ")
        onSchema(Field)(using ctx.nest)(extract(a))
      }
      ctx.ln
    }
    ctx.indent.append(")")
  }

  def onSumType[A](name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int): Out[A] = a => {
    val subtype = elements(toOrdinal(a))
    import subtype.{Underlying as Subtype, *}
    ctx.append("(")
    onSchema(Subtype)(cast(a))
    ctx.append(" : ").append(name.fullName).append(")")
  }

  def onNewType[A, B](name: TypeName[A], to: A => B, from: B => A, implementation: Schema[B]): Out[A] = a => {
    onSchema(implementation)(to(a))
    ctx.append(".asInstanceOf[").append(name.fullName).append("]")
  }
  def onSeqLike[A, B](
      name: TypeName[A],
      to: A => IterableOnce[B],
      from: IterableOnce[B] => A,
      item: Schema[B]
  ): Out[A] = a => {
    val it = to(a).iterator
    ctx.append(name.fullName).append("(")
    if it.nonEmpty then {
      it.zipWithIndex.foreach { case (c, i) =>
        if i > 0 then {
          ctx.append(",")
        }
        ctx.ln.nest.indent
        onSchema(item)(using ctx.nest)(c)
      }
      ctx.ln
    }
    ctx.append(")")
  }
  def onMapLike[A, K, V](
      name: TypeName[A],
      to: A => IterableOnce[(K, V)],
      from: IterableOnce[(K, V)] => A,
      key: Schema[K],
      value: Schema[V]
  ): Out[A] = a => {
    val it = to(a).iterator
    ctx.append(name.fullName).append("(")
    if it.nonEmpty then {
      it.zipWithIndex.foreach { case ((k, v), i) =>
        if i > 0 then {
          ctx.append(",")
        }
        ctx.ln.nest.indent
        onSchema(key)(using ctx.nest)(k)
        ctx.append(" -> ")
        onSchema(value)(using ctx.nest)(v)
      }
      ctx.ln
    }
    ctx.append(")")
  }
  def onErased[A, B](name: TypeName[A], to: A => B, from: B => A, cause: String): Out[A] = a =>
    ctx.append(name.fullName).append(" [").append(cause).append("]")
}
