/** Type checking with variability.
  */
package de.uni_saarland.cs.se

import Constant.*
import Type.*
import VExpression.*

import cafesat.api.Formulas.Formula
import cafesat.api.{Formulas, Solver}

/** Variability context as in lecture slides 74.
  */
case class VariabilityContext(formula: Formula) {

  /** Make equality consider logical equality of formulas.
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: VariabilityContext =>
        Solver.solveForSatisfiability(!(formula iff other.formula)) match {
          case Some(_) => false
          case None    => true
        }
      case _ => false
    }
  }

  override def toString: String = formula.toString
}

object VariabilityContext {

  /** Creates an empty variability context.
    */
  def emptyContext(): VariabilityContext = new VariabilityContext(Formulas.True)

  /** Allow implicit conversion from formulas to `VariabilityContext`.
    */
  given formulaToVarCtx: Conversion[Formula, VariabilityContext] =
    VariabilityContext(_)
}

/** Type alias for type context type */
type VTypeContext = TypeContext[Identifier, VType]

/** Type alias for context (= variability context + type context) type */
type VContext = (VariabilityContext, VTypeContext)

/** Type alias for result type */
type VResult = TypeCheckResult[VExpression, VType, VContext]

object VariabilityTypeChecker {

  /** Type-check a single expression.
    */
  def checkType(
      expr: VExpression,
      context: VContext = createContext()
  ): VResult = {
    new VariabilityTypeChecker().checkType(expr, context)
  }

  /** Simplify creation of variability context + type context.
    */
  def createContext(
      variabilityContext: VariabilityContext =
        VariabilityContext.emptyContext(),
      typeContext: VTypeContext = TypeContext()
  ): VContext = (variabilityContext, typeContext)
}

/** Type checker implementation for the language with variability.
  */
class VariabilityTypeChecker extends TypeChecker[VExpression, VType, VContext] {

  override def checkType(expr: VExpression, context: VContext): VResult = {
    val (variabilityContext, typeContext) = context
    expr match {
      case Const(c) => {
        val t = c match {
          case True | False => BoolTy
          case Num(_)       => NumTy
        }
        Success(VType(t -> variabilityContext.formula))
      }
      case Id(id) => {
        typeContext.typeForVar(id) match {
          case None => Failure(expr, context, s"Unknown identifier $id")
          case Some(varType) => {
            Solver.solveForSatisfiability(
              variabilityContext.formula && !varType.theta()
            ) match {
              case Some(_) =>
                Failure(expr, context, s"Type of $id is not defined in context")
              case None =>
                Success(
                  new VType(
                    varType.types
                      .map({ case (t: Type, pc: Formula) =>
                        (t, pc && variabilityContext.formula)
                      })
                  )
                )
            }
          }
        }
      }
      case Smaller(lhs, rhs) => {
        (checkType(lhs, context), checkType(rhs, context)) match {
          case (Success(lhsType), Success(rhsType)) => {
            (
              lhsType.formulaForType(NumTy),
              rhsType.formulaForType(NumTy)
            ) match {
              case (Some(lhsFormula), Some(rhsFormula)) => {
                Solver.solveForSatisfiability(
                  variabilityContext.formula && !(lhsFormula && rhsFormula)
                ) match {
                  case Some(_) =>
                    Failure(
                      expr,
                      context,
                      s"Either $lhs or $rhs is not of type NumTy in context"
                    )
                  case None =>
                    Success(VType(BoolTy -> variabilityContext.formula))
                }
              }
              case _ =>
                Failure(
                  expr,
                  context,
                  s"Either $lhs or $rhs is not of type NumTy in context"
                )
            }
          }
          case (Failure(_, _, msg), _) => Failure(expr, context, msg)
          case (_, Failure(_, _, msg)) => Failure(expr, context, msg)
        }
      }
      case If(condition, thenExpr, elseExpr) => {
        (
          checkType(condition, context),
          checkType(thenExpr, context),
          checkType(elseExpr, context)
        ) match {
          case (
                Success(conditionType),
                Success(thenType),
                Success(elseType)
              ) => {
            conditionType.formulaForType(BoolTy) match {
              case None =>
                Failure(
                  expr,
                  context,
                  s"Condition $condition is not of type BoolTy in context"
                )
              case Some(conditionFormula) => {
                Solver.solveForSatisfiability(
                  variabilityContext.formula && !conditionFormula
                ) match {
                  case Some(_) =>
                    Failure(
                      expr,
                      context,
                      s"Condition $condition is not of type BoolTy in context"
                    )
                  case None => {
                    val thenFormula = thenType.theta()
                    val elseFormula = elseType.theta()
                    Solver.solveForSatisfiability(
                      variabilityContext.formula && !(thenFormula && elseFormula)
                    ) match {
                      case Some(_) =>
                        Failure(
                          expr,
                          context,
                          s"Type of either $thenExpr or $elseExpr is not defined in context"
                        )
                      case None => {
                        if (thenType == elseType) {
                          Success(thenType.join(elseType))
                        } else {
                          Failure(
                            expr,
                            context,
                            s"expected both branches to have the same type. Type of $thenExpr and $elseExpr do not match"
                          )
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          case (Failure(_, _, msg), _, _) => Failure(expr, context, msg)
          case (_, Failure(_, _, msg), _) => Failure(expr, context, msg)
          case (_, _, Failure(_, _, msg)) => Failure(expr, context, msg)
        }
      }
      case Let(variable, varValue, inExpr) => {
        typeContext.typeForVar(variable) match {
          case Some(_) =>
            Failure(
              expr,
              context,
              s"Variable $variable is already defined in context"
            )
          case None => {
            checkType(varValue, context) match {
              case Failure(_, _, msg) => Failure(expr, context, msg)
              case Success(varType) => {
                Solver.solveForSatisfiability(
                  variabilityContext.formula && !varType.theta()
                ) match {
                  case Some(_) =>
                    Failure(
                      expr,
                      context,
                      s"Type of $varValue is not defined in context"
                    )
                  case None => {
                    checkType(
                      inExpr,
                      (
                        variabilityContext,
                        typeContext.withVar(variable, varType)
                      )
                    ) match {
                      case Failure(_, _, msg) => Failure(expr, context, msg)
                      case Success(exprType) => {
                        Solver.solveForSatisfiability(
                          variabilityContext.formula && !exprType.theta()
                        ) match {
                          case Some(_) =>
                            Failure(
                              expr,
                              context,
                              s"Type of $inExpr is not defined in context"
                            )
                          case None => {
                            Success(exprType)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      case Choice(phi, trueChoice, falseChoice) => {
        (
          checkType(
            trueChoice,
            (variabilityContext.formula && phi, typeContext)
          ),
          checkType(
            falseChoice,
            (variabilityContext.formula && !phi, typeContext)
          )
        ) match {
          case (Success(trueType), Success(falseType)) =>
            Success(trueType.join(falseType))
          case (Failure(_, _, msg), _) => Failure(expr, context, msg)
          case (_, Failure(_, _, msg)) => Failure(expr, context, msg)
        }
      }
    }
  }
}
