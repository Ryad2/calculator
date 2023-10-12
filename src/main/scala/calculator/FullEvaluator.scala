package calculator

import scala.util.{Try, Success, Failure}

object FullEvaluator:
  /** Result of evaluation. */
  enum FullEvalResult:
    case Ok(v: Double)
    case DivByZero
    case UndefinedVar(name: String)

    def get: Double = this match
      case Ok(v)              => v
      case DivByZero          => throw new RuntimeException("division by zero")
      case UndefinedVar(name) => throw new RuntimeException(s"undefined variable: $name")

  // Define your own context here

  enum MyContext:
    case Empty
    case Cons(name: String, value: Double, tail: MyContext)
    
  type Ctx =
    MyContext

  def empty: Ctx =
    MyContext.Empty

  def cons(name: String, value: Double, tail: Ctx) =
    MyContext.Cons(name, value, tail)
      
  enum LookupResults :
    case NotFound
    case Here(value: Double)
    
  def lookup(name: String, ctx: Ctx): LookupResults =
    ctx match
      case MyContext.Empty => LookupResults.NotFound
      case MyContext.Cons(n, v, t) =>
        if n == name then LookupResults.Here(v)
        else lookup(name, t)

  def fromList(xs: List[(String, Double)]): Ctx =
    xs match
      case Nil           => empty
      case (n, v) :: rem => cons(n, v, fromList(rem))

class FullEvaluator(ctx: FullEvaluator.Ctx) extends Evaluator[FullExpr, FullEvaluator.FullEvalResult]:
  import FullEvaluator.*
  import FullExpr.*
  import FullEvalResult.*

  /** Evaluate an expression to its value. */
  def evaluate(e: FullExpr): FullEvalResult =
    e match
      case Number(v) => Ok(v)
      case Add(l, r) =>
        val lval = evaluate(l)
        val rval = evaluate(r)
        (lval, rval) match
          case (Ok(lv), Ok(rv)) => Ok(lv + rv)
          case (Ok(_), r)       => r
          case (l, _)           => l

      case Minus(l, r) =>
        val lval = evaluate(l)
        val rval = evaluate(r)
        (lval, rval) match
          case (Ok(lv), Ok(rv)) => Ok(lv - rv)
          case (Ok(_), r)       => r
          case (l, _)           => l

      case Mul(l, r) =>
        val lval = evaluate(l)
        val rval = evaluate(r)
        (lval, rval) match
          case (Ok(lv), Ok(rv)) => Ok(lv * rv)
          case (Ok(_), r)       => r
          case (l, _)           => l

      case Div(l, r) =>
        val lval = evaluate(l)
        val rval = evaluate(r)
        (lval, rval) match
          case (Ok(lv), Ok(rv)) =>
            if rv == 0 then DivByZero
            else Ok(lv / rv)
          case (Ok(_), r)       => r
          case (l, _)           => l

      case Var(name) =>
        lookup(name, ctx) match
          case LookupResults.NotFound => UndefinedVar(name)
          case LookupResults.Here(v)  => Ok(v)

      case Neg(e) =>
        val el = evaluate(e)
        el match
          case Ok(v) => Ok(-v)
          case _     => el