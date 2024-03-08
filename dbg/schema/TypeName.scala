package dbg.schema

final case class TypeName[A](fullName: String)

object TypeName {

  def apply[A](using A: TypeName[A]): TypeName[A] = A

  inline given derived[A]: TypeName[A] = ${ derivedImpl[A] }

  import scala.quoted.*

  def derivedImpl[A: Type](using Quotes): Expr[TypeName[A]] =
    import quotes.reflect.*
    val name = TypeRepr.of[A].show.takeWhile(_ != '[')
    '{ TypeName(${ summon[ToExpr[String]].apply(name) }) }
}

opaque type UntypedTypeName = String
extension (utn: UntypedTypeName) def of[A]: TypeName[A] = TypeName(utn)

extension (sc: StringContext)
  def typeName(args: TypeName[?]*): UntypedTypeName =
    sc.s(args.map(_.fullName))
