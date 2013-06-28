
trait Parser[T] extends (String => ParseResult[T]) {
  def ~[U](q: Parser[U]): Parser[(T, U)]= {
    val p = this
    new Parser[(T, U)] {
      override def apply(input: String): ParseResult[(T, U)] =
        p(input) match {
          case ParseSuccess(value1, nextInput) =>
            q(nextInput) match {
              case ParseSuccess(value2, restInput) =>
                ParseSuccess((value1, value2), restInput)

              case ParseFailure(errorMessage, restInput) =>
                ParseFailure(errorMessage, restInput)
            }

          case ParseFailure(errorMessage, restInput) =>
            ParseFailure(errorMessage, restInput)
        }
    }
  }
}
