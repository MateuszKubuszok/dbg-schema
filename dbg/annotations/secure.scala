package dbg.annotations

import scala.annotation.StaticAnnotation
import scala.deriving.*
import scala.quoted.*

class secure extends StaticAnnotation
object secure {

  // Whether Tpe is annotated
  inline def isAnnotated[A]: Boolean = ${ mkIsAnnotated[A] }

  // Whether each element of ADT is annotated.
  inline def annotatedPositions[ADT]: List[Boolean] = ${ mkAnnotatedPositions[ADT] }

  def mkIsAnnotated[A: Type](using q: Quotes): Expr[Boolean] = {
    import q.reflect.*

    val tpeType = TypeRepr.of[A]
    val annType = TypeRepr.of[secure]

    tpeType.classSymbol.match {
      case Some(tpeCls) =>
        tpeCls.annotations.find(_.tpe <:< annType) match {
          case Some(_) => '{ true }
          case None    => '{ false }
        }
      case None => '{ false }
    }
  }

  def mkAnnotatedPositions[ADT: Type](using q: Quotes): Expr[List[Boolean]] = {
    import q.reflect.*

    val adtType = TypeRepr.of[ADT].dealias
    val annType = TypeRepr.of[secure].dealias

    val annSymbol = annType.typeSymbol

    adtType.classSymbol.match {
      case Some(adtSym) if adtSym.flags.is(Flags.Case) =>
        val fields: List[Expr[Boolean]] = adtSym.primaryConstructor.paramSymss
          .filter { params =>
            params.headOption.fold(true)(_.isTerm) // paramSymss might be List(List(type A), List(val x, val y, val z))
          }
          .flatten
          .map { field =>
            field.annotations.find(_.tpe <:< annType) match {
              case Some(_) => '{ true }
              case None    => '{ false }
            }
          }
        Expr.ofList(fields)
      case Some(adtSym) if adtSym.flags.is(Flags.Sealed) =>
        val subclasses: List[Expr[Boolean]] = adtSym.children.map { subsclass =>
          subsclass.annotations.find(_.tpe <:< annType) match {
            case Some(_) => '{ true }
            case None    => '{ false }
          }
        }
        Expr.ofList(subclasses)
      case _ =>
        report.throwError(s"Expected case class or a sealed trait (enum), got ${adtType.show}")
    }
  }
}
