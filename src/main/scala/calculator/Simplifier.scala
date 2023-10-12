package calculator

object Simplifier:
  import FullExpr.*

  /** Fold constant sub-expressions in values. */
  def constfold(e: FullExpr): FullExpr =
    e match

      case Number(v) => Number(v)

      case Add(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 + el2)
          case (Var(_), _) => Add(l, constfold(r))
          case (_, Var(_)) => Add(constfold(l), r)

          case _ => (constfold(l), constfold(r)) match
            case (Number(el1), Number(el2)) => Number(el1 + el2)
            case (left, right) => Add(left, right)


      case Minus(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 - el2)
          case (Var(_), _) => Minus(l, constfold(r))
          case (_, Var(_)) => Minus(constfold(l), r)

          case _ => (constfold(l), constfold(r)) match
            case (Number(el1), Number(el2)) => Number(el1 - el2)
            case (left, right) => Minus(left, right)

      case Mul(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          case (Var(_), _) => Mul(l, constfold(r))
          case (_, Var(_)) => Mul(constfold(l), r)

          case _ => (constfold(l), constfold(r)) match
            case (Number(el1), Number(el2)) => Number(el1 * el2)
            case (left, right) => Mul(left, right)

      case Div(l, r) =>
         (l, r) match
            case (Number(el1), Number(el2)) =>
              if el2 == 0 then Number(Double.NaN)
              else Number(el1 / el2)
            case (Var(_), _) => Div(l, constfold(r))
            case (_, Var(_)) => Div(constfold(l), r)

            case _ => (constfold(l), constfold(r)) match
              case (Number(el1), Number(el2)) =>
                if el2 == 0 then Number(Double.NaN)
                else Number(el1 / el2)
              case (left, right) => Mul(left, right)



      case Neg(e) =>
        e match
          case Number(v) => Number(-v)
          case Var(name) => Mul(Number(-1), Var(name))
          case _ => Neg(constfold(e))

      case Var(name) => Var(name)

  // simplification rules
  // 1. 0 + e = e + 0 = e
  // 2. 0 - e = -e
  // 3. e - 0 = e
  // 4. 0 * e = e * 0 = 0
  // 5. 1 * e = e * 1 = e
  // 6. e / 1 = e
  // 7. e - e = 0
  /** Simplifiy expressions based on the listed algebraic rules. */
  def algebraic(e: FullExpr): FullExpr =
    e match
      case Number(v) => Number(v)

      case Add(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 + el2)
          case ( _ , Number(0.0)) => l
          case (Number(0.0), _ ) => r
          case (Var(_), _) => Add(l, constfold(r))
          case (_, Var(_)) => Add(constfold(l), r)
          case _ => (algebraic(l), algebraic(r)) match
            case (Number(el1), Number(el2)) => Number(el1 + el2)
            case (left, right) => Add(left, right)


      case Minus(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 - el2)
          case (_, Number(0.0)) => l
          case (Number(0.0), _) => Neg(r)
          case (Var(n1), Var(n2)) if n1 == n2 => Number(0.0)
          case (Var(_), _) => Minus(l, algebraic(r))
          case (_, Var(_)) => Minus(algebraic(l), r)

          case _ => (algebraic(l), algebraic(r)) match
            case (Number(el1), Number(el2)) => Number(el1 - el2)
            case (left, right) => Minus(left, right)

      case Mul(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          case (_, Number(0.0)) => Number(0.0)
          case (Number(0.0), _) => Number(0.0)
          case (_, Number(1.0)) => l
          case (Number(1.0), _) => r
          case (Var(_), _) => Mul(l, algebraic(r))
          case (_, Var(_)) => Mul(algebraic(l), r)

          case _ => (algebraic(l), algebraic(r)) match
            case (Number(el1), Number(el2)) => Number(el1 * el2)
            case (left, right) => Mul(left, right)

      case Div(l, r) =>
        (l, r) match
          case (_, Number(1.0)) => l
          case _ => (constfold(l), constfold(r)) match
            case (Number(el1), Number(el2)) =>
              if el2 == 0 then Number(Double.NaN)
              else Number(el1 / el2)
            case (left, right) => Mul(left, right)


      case Neg(e) =>
        e match
        case Number(v) => Number(-v)
        case Var(name) => Mul(Number(-1), Var(name))
        case _ => Neg(algebraic(e))


      case Var(name) => Var(name)

  def simplify(e: FullExpr): FullExpr =
    e
