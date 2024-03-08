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
        i -= 1
        append(indentation)
      }
      this
    }

    def ln: Context = append(lineSeparation)

    def nest: Context = copy(nesting = nesting + 1)
  }
  inline def ctx(using ctx: Context): Context = ctx

  type Out[A] = A => Context

  def prettyPrint[A: Schema](
      value: A,
      indentation: String = "  ",
      lineSeparation: String = "\n",
      nesting: Int = 0
  ): String =
    onSchema(Schema[A])(using
      Context(
        indentation = indentation,
        lineSeparation = lineSeparation,
        nesting = nesting,
        builder = new StringBuilder
      )
    )(value).builder.toString()

  def onNothing(using Context): Out[Nothing] = _ => ctx.append("???")
  def onNull(using Context): Out[Null]       = _ => ctx.append("null")
  def onUnit(using Context): Out[Unit]       = _ => ctx.append("()")
  def onByte(using Context): Out[Byte]       = a => ctx.append(a.toString + ".toByte")
  def onBoolean(using Context): Out[Boolean] = a => ctx.append(a.toString)
  def onShort(using Context): Out[Short]     = a => ctx.append(a.toString + ".toShort")
  def onInt(using Context): Out[Int]         = a => ctx.append(a.toString)
  def onLong(using Context): Out[Long]       = a => ctx.append(a.toString + "L")
  def onFloat(using Context): Out[Float]     = a => ctx.append(a.toString + "f")
  def onDouble(using Context): Out[Double]   = a => ctx.append(a.toString)
  def onChar(using Context): Out[Char]       = a => ctx.append("'" + a.toString + "'")
  def onString(using Context): Out[String]   = a => ctx.append("\"" + a.toString + "\"")

  def onSingleton[A](name: TypeName[A], value: A)(using Context): Out[A] = _ =>
    ctx.indent.append(name.fullName + ".type")

  def onProduct[A](name: TypeName[A], fields: IArray[Field.Of[A]], construct: IArray[Any] => A)(using Context): Out[A] =
    a => {
      ctx.append(name.fullName)
      if fields.isEmpty then {
        ctx.append("()")
      } else {
        ctx.append("(")
        fields.zipWithIndex.foreach { case (field, i) =>
          if i > 0 then {
            ctx.append(",")
          }
          import field.{Underlying as Field, *}
          ctx.ln.nest.indent.append(label).append(" = ")
          onSchema(Field)(using ctx.nest)(extract(a))
        }
        ctx.ln.append(")")
      }
    }

  def onSumType[A](name: TypeName[A], elements: IArray[Subtype.Of[A]], toOrdinal: A => Int)(using Context): Out[A] =
    a => {
      val subtype = elements(toOrdinal(a))
      import subtype.{Underlying as Subtype, *}
      ctx.append("(")
      onSchema(Subtype)(cast(a))
      ctx.append(" : ").append(name.fullName).append(")")
    }

  def onInvariant[A, B](name: TypeName[A], to: A => B, from: B => A, hint: MappingType[A, B])(using Context): Out[A] =
    a => {
      lazy val b = to(a)
      hint match {
        case MappingType.NewType(implementation) =>
          onSchema(implementation)(b)
          ctx.append(".asInstanceOf[").append(name.fullName).append("]")
        case seqLike: MappingType.SeqLike[?, c] =>
          val it = b.iterator
          ctx.append(name.fullName)
          if it.isEmpty then {
            ctx.append("()")
          } else {
            ctx.append("(")
            it.zipWithIndex.foreach { case (c, i) =>
              if i > 0 then {
                ctx.append(",")
              }
              ctx.ln.nest.indent
              onSchema(seqLike.item)(using ctx.nest)(c)
            }
            ctx.ln.append(")")
          }
        case mapLike: MappingType.MapLike[?, k, v] =>
          val it = b.iterator
          ctx.append(name.fullName)
          if it.isEmpty then {
            ctx.append("()")
          } else {
            ctx.append("(")
            it.zipWithIndex.foreach { case ((k, v), i) =>
              if i > 0 then {
                ctx.append(",")
              }
              ctx.ln.nest.indent
              onSchema(mapLike.key)(using ctx.nest)(k)
              ctx.append(" -> ")
              onSchema(mapLike.value)(using ctx.nest)(v)
            }
            ctx.ln.append(")")
          }
        case MappingType.Erased(cause) => ctx.append(name.fullName).append(" [").append(cause).append("]")
      }
    }
}
