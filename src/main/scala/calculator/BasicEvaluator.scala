package calculator

class BasicEvaluator extends Evaluator[BasicExpr, BasicEvaluator.EvalResult]:
  import BasicExpr.*
  import BasicEvaluator.*
  import EvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: BasicExpr): EvalResult =
    e match
      case Number(v) => Ok(v)
      case Add(l, r) =>
        val left = evaluate(l)
        val right = evaluate(r)
        (left, right) match
          case (Ok(lv), Ok(rv)) => Ok(lv + rv)
          case _               => DivByZero
      case Minus(l, r) =>
        val left = evaluate(l)
        val right = evaluate(r)
        (left, right) match
          case (Ok(lv), Ok(rv)) => Ok(lv - rv)
          case _ => DivByZero

      case Mul(l, r) =>
        val left = evaluate(l)
        val right = evaluate(r)
        (left, right) match
          case (Ok(lv), Ok(rv)) => Ok(lv * rv)
          case _ => DivByZero

      case Neg(e) =>
        evaluate(e) match
          case Ok(v) => Ok(-v)
          case DivByZero => DivByZero

      case Div(l, r) =>
        val left = evaluate(l)
        val right = evaluate(r)

        (left, right) match
          case (Ok(lv), Ok(rv)) =>
            if rv == 0 then DivByZero
            else Ok(lv / rv)
          case _ => DivByZero

object BasicEvaluator:
  enum EvalResult:
    case Ok(v: Double)
    case DivByZero

    def get: Double = this match
      case Ok(v)     => v
      case DivByZero => throw new RuntimeException(s"division by zero")
