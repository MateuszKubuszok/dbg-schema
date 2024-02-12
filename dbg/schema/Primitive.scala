package dbg.schema

enum Primitive[A] {
  case Nothing extends Primitive[scala.Nothing]
  case Null    extends Primitive[scala.Null]
  case Unit    extends Primitive[scala.Unit]
  case Byte    extends Primitive[scala.Byte]
  case Boolean extends Primitive[scala.Boolean]
  case Short   extends Primitive[scala.Short]
  case Int     extends Primitive[scala.Int]
  case Long    extends Primitive[scala.Long]
  case Float   extends Primitive[scala.Float]
  case Double  extends Primitive[scala.Double]
  case Char    extends Primitive[scala.Char]
  case String  extends Primitive[scala.Predef.String]
}
