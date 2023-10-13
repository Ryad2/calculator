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
              case (left, right) => Div(left, right)



      case Neg(e) =>
        e match
          case Number(v) => Number(-v)
          case Var(name) => Mul(Number(-1), Var(name))
          case _ => Neg(constfold(e))

      case Var(name) => Var(name)

  /*// simplification rules
  // 1. 0 + e = e + 0 = e
  // 2. 0 - e = -e
  // 3. e - 0 = e
  // 4. 0 * e = e * 0 = 0
  // 5. 1 * e = e * 1 = e
  // 6. e / 1 = e
  // 7. e - e = 0
  /** Simplifiy expressions based on the listed algebraic rules. */

   */
/*  def algebraic(e: FullExpr): FullExpr =
    e match
      case Number(v) => Number(v)

      case Add(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 + el2)

          case _ => (algebraic(l), algebraic(r)) match
            case (_, Number(0.0)) => algebraic(l)
            case (Number(0.0), _) => algebraic(r)
            case (Number(el1), Number(el2)) => Number(el1 + el2)
            case (left, right) => Add(left, right)


      case Minus(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 - el2)
          case ( _, Number(0.0)) => algebraic(l)
          case (Number(0.0), _) => Neg( r )

          //case (Var(_), _) => Minus(l, algebraic(r))
          //case (_, Var(_)) => Minus(algebraic(l), r)

          case _ => (algebraic(l), algebraic(r)) match
            case (Number(el1), Number(el2)) => Number(el1 - el2)
            case ( _, Number(0.0)) => algebraic(l)
            case (Number(0.0), _) => Neg(algebraic(r))
            case (left, right) =>
              if left == right then Number(0.0) else Minus(left, right)


      /*case Mul(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          case ( _, Number(0.0)) => Number(0.0)
          case (Number(0.0), _) => Number(0.0)

          case _ => (algebraic(l), algebraic(r)) match

            case (Number(el1), Number(el2)) => Number(el1 * el2)
            case (el, Number(1.0)) => algebraic(el)
            case (Number(1.0), el) => algebraic(el)
            case (_, Number(0.0)) => Number(0.0)
            case (Number(0.0), _) => Number(0.0)
            case (left, right) => Mul( algebraic(left), algebraic(right))
*/
      case Mul(l, r) => (l, r) match
        case (Var(_), Add(Number(_), Number(_))) => Mul(l, algebraic(r))
        case (Number(el1), Number(el2)) => Number(el1 * el2)
        case (_, Number(0.0)) => Number(0.0)
        case (Number(0.0), _) => Number(0.0)
        case _ => (algebraic(l), algebraic(r)) match
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          case (el, Number(1.0)) => algebraic(el)
          case (Number(1.0), el) => algebraic(el)
          case (_, Number(0.0)) => Number(0.0)
          case (Number(0.0), _) => Number(0.0)
          case (left, right) => Mul(left, right)


      case Div(l, r) =>
        (l, r) match
          case (Number(el1), Number(el2)) =>
            if el2 == 0 then Number(Double.NaN)
            else Number(el1 / el2)


          //case (Var(_), _) => Div(l, algebraic(r))
          //case (_, Var(_)) => Div(algebraic(l), r)

          case _ => (algebraic(l), algebraic(r)) match
            case (Number(el1), Number(el2)) =>
              if el2 == 0 then Number(Double.NaN)
              else Number(el1 / el2)
            case ( _, Number(1.0)) => l
            case (left, right) => Div(left, right)


      case Neg(e) =>
        e match
        case Number(v) => Number(-v)
        case Var(name) => Mul(Number(-1), Var(name))
        case _ => Neg(algebraic(e))


      case Var(name) => Var(name)*/



  def algebraic(e: FullExpr): FullExpr =
    e match
      case Number(v) => Number(v)

      case Add(l, r) =>
        (algebraic(l), algebraic(r)) match
          case (Number(0.0), r) => r
          case (l, Number(0.0)) => l
          case (Number(el1), Number(el2)) => Number(el1 + el2)
          case (left, right) => Add(left, right)

      case Minus(l, r) =>
        (algebraic(l), algebraic(r)) match
          case (Number(0.0), r) => Neg(r)
          case (l, Number(0.0)) => l
          case (Number(el1), Number(el2)) => Number(el1 - el2)
          case (left, right) => if left == right then Number(0.0) else Minus(left, right)

      case Mul(l, r) =>
        (algebraic(l), algebraic(r)) match
          //case (Var(_), el) => Mul(algebraic(l), el)
          //case (Add(Number(_), Number(_)), Var(_)) => Mul(algebraic(l), r)
          case (_, Number(0.0)) | (Number(0.0), _) => Number(0.0)
          case (el, Number(1.0)) => el
          case (Number(1.0), el) => el
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          //todo weird lines
          case (left, right) => if left.isInstanceOf[Var] then Mul(left, r) else Mul(left, right)

      case Div(l, r) =>
        (algebraic(l), algebraic(r)) match
          case (_, Number(0.0)) => Number(Double.NaN)
          case (_, Number(1.0)) => l
          case (Number(el1), Number(el2)) => Number(el1 / el2)
          case (left, right) => Div(left, right)

      case Neg(e) =>
        e match
          case Number(v) => Number(-v)
          case Var(name) => Mul(Number(-1), Var(name))
          case _ => Neg(algebraic(e))

      case Var(name) => Var(name)


  /** Simplify expressions based on the listed trigonometric rules. */

  def simplify(e: FullExpr): FullExpr =
    e match
      case Number(v) => Number(v)

      case Add(l, r) =>
        val simplifiedL = simplify(l)
        val simplifiedR = simplify(r)
        (simplifiedL, simplifiedR) match
          case (Number(0.0), _) | (Neg(Number(0.0)), _) => simplifiedR
          case (_, Number(0.0)) | (_, Neg(Number(0.0))) => simplifiedL
          case (Number(el1), Number(el2)) => Number(el1 + el2)
          case _ => Add(simplifiedL, simplifiedR)

      case Minus(l, r) =>
        val simplifiedL = simplify(l)
        val simplifiedR = simplify(r)
        (simplifiedL, simplifiedR) match
          case (Number(0.0), Number(0.0)) => Number(0.0)
          case (Number(0.0), _) => Neg(simplifiedR)
          case (_, Number(0.0)) => simplifiedL
          case (Number(el1), Number(el2)) => Number(el1 - el2)
          case _ => if simplifiedL == simplifiedR then Number(0.0) else Minus(simplifiedL, simplifiedR)

      case Mul(l, r) =>
        val simplifiedL = simplify(l)
        val simplifiedR = simplify(r)
        (simplifiedL, simplifiedR) match
          case (Number(1.0), _) => simplifiedR
          case (_, Number(1.0)) => simplifiedL
          case (Number(0.0), _) | (_, Number(0.0)) => Number(0.0)
          case (Number(el1), Number(el2)) => Number(el1 * el2)
          case _ => Mul(simplifiedL, simplifiedR)

      case Div(l, r) =>
        val simplifiedL = simplify(l)
        val simplifiedR = simplify(r)
        (simplifiedL, simplifiedR) match
          case (_, Number(1.0)) => simplifiedL
          case (Number(el1), Number(el2)) =>
            if el2 == 0 then Number(Double.NaN)
            else Number(el1 / el2)
          case _ => Div(simplifiedL, simplifiedR)

      case Neg(e) =>
        simplify(e) match
          case Number(v) => if (v == 0.0) Number(0.0) else Number(-v)
          case Var(name) => Mul(Number(-1), Var(name))
          case _ => Neg(simplify(e))

      case Var(name) => Var(name)
