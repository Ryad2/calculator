package calculator

import FullExpr.*

enum ExprDiff:
  case Unchanged
  case Changed(to: Expr)
  case BinOp(e1: ExprDiff, e2: ExprDiff)
  case UnaryOp(e: ExprDiff)

  def <*>(other: ExprDiff): ExprDiff =
    (this, other) match
      case (Unchanged, Unchanged) => Unchanged
      case (d1, d2)               => BinOp(d1, d2)

object ExprDiff:
  /** Compute the difference between two expression trees. */
  def diff(e1: Expr, e2: Expr): ExprDiff =
    (e1, e2) match
      case (e1, e2) if e1 == e2                            => Unchanged
      case (Add(e11, e12), e2) if e12 == e2 || e11 == e2   => Changed(e2)
      case (Minus(e11, e12), e2) if e12 == e2 || e11 == e2 => Changed(e2)
      case (Mul(e11, e12), e2) if e12 == e2 || e11 == e2   => Changed(e2)
      case (Div(e11, e12), e2) if e12 == e2 || e11 == e2   => Changed(e2)
      case (Add(e11, e12), Add(e21, e22))                  => diff(e11, e21) <*> diff(e12, e22)
      case (Minus(e11, e12), Minus(e21, e22))              => diff(e11, e21) <*> diff(e12, e22)
      case (Mul(e11, e12), Mul(e21, e22))                  => diff(e11, e21) <*> diff(e12, e22)
      case (Div(e11, e12), Div(e21, e22))                  => diff(e11, e21) <*> diff(e12, e22)
      case (Neg(e1), Neg(e2)) => diff(e1, e2) match
          case Unchanged => Unchanged
          case diff      => UnaryOp(diff)
      case (e1, e2) => Changed(e2)
