package calculator

object SmallStepEvaluator:
  import TinyExpr.*

  /** Evaluate the expression by one step. Return the expression as it is if it
    * has been fully evaluated.
    */
  def step(e: TinyExpr): TinyExpr =
    e match
      case Number(value) => Number(value)
      
      case Mul(e1, e2) => (e1, e2) match
        case (Number(v1), Number(v2)) => Number(v1 * v2)
        case (Number(v1), e2) => Mul(Number(v1), step(e2))
        case (e1, e2) => Mul(step(e1), e2)
        
      case Add(e1, e2) => (e1, e2) match
        case (Number(v1), Number(v2)) => Number(v1 + v2)
        case (Number(v1), e2) => Add(Number(v1), step(e2))
        case (e1, e2) => Add(step(e1), e2)
    
      case Minus(e1, e2)
        => (e1, e2) match
          case (Number(v1), Number(v2)) => Number(v1 - v2)
          case (Number(v1), e2) => Minus(Number(v1), step(e2))
          case (e1, e2) => Minus(step(e1), e2)
      case Neg(e) => e match
        case Number(v) => Number(-v)
        case e => Neg(step(e))
