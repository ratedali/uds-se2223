/** Type checking without variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Expression.*
import Type.*

/** Type context as in lecture slides 71/74.
  *
  * @tparam IdT
  *   type used for variables
  * @tparam TypeT
  *   the types of the language
  */
case class TypeContext[IdT, TypeT] private (mapping: Map[IdT, TypeT]) {

  /** Create an extended copy of this type context that sets the type for the
    * given variable.
    */
  def withVar(id: IdT, value: TypeT): TypeContext[IdT, TypeT] = {
    new TypeContext(mapping updated (id, value))
  }

  /** Get the type for a given variable.
    */
  def typeForVar(id: IdT): Option[TypeT] = mapping.get(id)

  override def toString: String = {
    mapping.toSeq
      .map({ case (id: IdT, t: TypeT) =>
        s"($id: $t)"
      })
      .mkString("\n")
  }
}

object TypeContext {

  /** Magic function so that we can write `TypeContext(("x", VType(BoolTy -> *
    * A)))` instead of `new TypeContext(Map("x", VType(BoolTy -> A)))`.
    */
  def apply[IdT, TypeT](elems: (IdT, TypeT)*): TypeContext[IdT, TypeT] =
    new TypeContext(Map.from(elems))
}

/** Type alias for context type, i.e., the type context. */
type Context = TypeContext[Identifier, Type]

/** Type alias for result type. */
type Result = TypeCheckResult[Expression, Type, Context]

object SimpleTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: Expression,
      context: Context = TypeContext()
  ): Result = {
    new SimpleTypeChecker().checkType(expr, context)
  }
}

/** Type checker implementation for the language without variability.
  */
class SimpleTypeChecker extends TypeChecker[Expression, Type, Context] {

  override def checkType(expr: Expression, context: Context): Result = {
    expr match {
      case Const(c) => {
        val t = c match {
          case True | False => BoolTy
          case Num(_)       => NumTy
        }
        Success(t)
      }
      case Id(id) =>
        context.typeForVar(id) match {
          case Some(t) => Success(t)
          case None    => Failure(expr, context, s"Unknown identifier $id")
        }
      case Smaller(lhs, rhs) =>
        (checkType(lhs, context), checkType(rhs, context)) match {
          case (Success(NumTy), Success(NumTy)) =>
            Success(BoolTy)
          case (Success(NumTy), Success(rhsType)) =>
            Failure(
              expr,
              context,
              s"Type mismatch: expected $rhs to be NumTy, got $rhsType"
            )
          case (Success(lhsType), Success(_)) =>
            Failure(
              expr,
              context,
              s"Type mismatch: expected $lhs to be NumTy, got $lhsType"
            )
          case (Failure(_, _, msg), _) => Failure(expr, context, msg)
          case (_, Failure(_, _, msg)) => Failure(expr, context, msg)
        }
      case If(cond, thenExpr, elseExpr) =>
        (
          checkType(cond, context),
          checkType(thenExpr, context),
          checkType(elseExpr, context)
        ) match {
          case (
                Success(BoolTy),
                Success(thenType),
                Success(elseType)
              ) if thenType == elseType =>
            Success(thenType)
          case (
                Success(BoolTy),
                Success(thenType),
                Success(elseType)
              ) =>
            Failure(
              expr,
              context,
              s"Type mismatch: expected both branches to have the same type, got $thenType and $elseType"
            )
          case (
                Success(condType),
                Success(_),
                Success(_)
              ) =>
            Failure(
              expr,
              context,
              s"Type mismatch: expected $cond to be BoolTy, got $condType"
            )
          case (Failure(_, _, msg), _, _) =>
            Failure(expr, context, msg)
          case (_, Failure(_, _, msg), _) =>
            Failure(expr, context, msg)
          case (_, _, Failure(_, _, msg)) =>
            Failure(expr, context, msg)
        }
      case Let(variable, varValue, inExpr) => {
        context.typeForVar(variable) match {
          case Some(_) =>
            Failure(
              expr,
              context,
              s"$variable is already defined in this context"
            )
          case None => {
            checkType(varValue, context) match {
              case Success(varType) =>
                checkType(inExpr, context.withVar(variable, varType))
              case Failure(_, _, msg) => Failure(expr, context, msg)
            }
          }
        }
      }
    }

  }
}
