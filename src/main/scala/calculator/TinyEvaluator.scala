package calculator

class TinyEvaluator extends Evaluator[TinyExpr, Double]:
  import TinyExpr.*

  /** Evaluate an expression to its value. */
  def evaluate(e: TinyExpr): Double =
    e match
      case Number(value) => value
      case Mul(e1, e2) => (e1, e2) match
        case (Number(v1), Number(v2)) => v1 * v2
        case (e1, e2) => evaluate(e1) * evaluate(e2)

      case Add(e1, e2) => (e1, e2) match
        case (Number(v1), Number(v2)) => v1 + v2
        case (e1, e2) => evaluate(e1) + evaluate(e2)


      case Minus(e1, e2) => (e1, e2) match
        case (Number(v1), Number(v2)) => v1 - v2
        case (e1, e2) => evaluate(e1) - evaluate(e2)

      case Neg(e) => e match
        case Number(v) => -v
        case e => -evaluate(e)