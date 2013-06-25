
object Combinators {
  def follows[T1, T2](p1: Parser[T1], p2: Parser[T2]) =
    new Parser[(T1, T2)] {
      override def apply(input: String) =
        p1(input) match {
          case Success(v1, nextInput) =>
            p2(nextInput) match {
              case Success(v2, restInput) =>
                Success((v1, v2), restInput)

              case Failure(errorMessage, restInput) =>
                Failure(errorMessage, input)
            }

          case Failure(errorMessage, restInput) =>
            Failure(errorMessage, input)
        }
    }

  def rep[T](p: Parser[T]): Parser[List[T]] =
    new Parser[List[T]] {
      override def apply(input: String) =
        p(input) match {
          case Success(v, nextInput) =>
            apply(nextInput) match {
              case Success(vs, restInput) =>
                Success(v :: vs, restInput)

              case Failure(_, restInput) =>
                Success(List(), restInput)
            }

          case Failure(_, restInput) => Success(List(), restInput)
        }
    }

  def applyF[T1, T2](p: Parser[T1], f: T1 => T2) =
    new Parser[T2] {
      override def apply(input: String) =
        p(input) match {
          case Success(value, restInput) => Success(f(value), restInput)
          case Failure(message, restInput) => Failure(message, restInput)
        }
    }
}
