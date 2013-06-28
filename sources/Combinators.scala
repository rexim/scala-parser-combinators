
object Combinators {
  def follows[T1, T2](p1: Parser[T1], p2: Parser[T2]) =
    new Parser[(T1, T2)] {
      override def apply(input: String) =
        p1(input) match {
          case ParseSuccess(v1, nextInput) =>
            p2(nextInput) match {
              case ParseSuccess(v2, restInput) =>
                ParseSuccess((v1, v2), restInput)

              case ParseFailure(errorMessage, restInput) =>
                ParseFailure(errorMessage, input)
            }

          case ParseFailure(errorMessage, restInput) =>
            ParseFailure(errorMessage, input)
        }
    }

  def rep[T](p: Parser[T]): Parser[List[T]] =
    new Parser[List[T]] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(v, nextInput) =>
            apply(nextInput) match {
              case ParseSuccess(vs, restInput) =>
                ParseSuccess(v :: vs, restInput)

              case ParseFailure(_, restInput) =>
                ParseSuccess(List(), restInput)
            }

          case ParseFailure(_, restInput) => ParseSuccess(List(), restInput)
        }
    }

  def applyF[T1, T2](p: Parser[T1], f: T1 => T2) =
    new Parser[T2] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(value, restInput) => ParseSuccess(f(value), restInput)
          case ParseFailure(message, restInput) => ParseFailure(message, restInput)
        }
    }
}
