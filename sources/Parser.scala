import scala.language.postfixOps

trait Parser[+T] extends (String => ParseResult[T]) {
  def ~[U](q: => Parser[U]): Parser[(T, U)] = {
    val p = this
    new Parser[(T, U)] {
      override def apply(input: String) =
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

  def ~>[U](q: => Parser[U]): Parser[U] =
    this ~ q ^^ {
      case (_, result) => result
    }

  def <~[U](q: => Parser[U]): Parser[T] =
    this ~ q ^^ {
      case (result, _) => result
    }

  def |[U >: T](q: => Parser[U]): Parser[U] = {
    val p = this

    new Parser[U] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(value, restInput) => ParseSuccess(value, restInput)
          case ParseFailure(_, _) => q(input)
        }
    }
  }

  def ?(): Parser[Option[T]] = {
    val p = this
    new Parser[Option[T]] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(value, restInput) =>
            ParseSuccess(Some(value), restInput)

          case ParseFailure(_, restInput) =>
            ParseSuccess(None, restInput)
        }
    }
  }

  def *(): Parser[List[T]] = {
    val p = this
    new Parser[List[T]] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(value, nextInput) =>
            apply(nextInput) match {
              case ParseSuccess(values, restInput) =>
                ParseSuccess(value :: values, restInput)

              case ParseFailure(_, restInput) =>
                ParseSuccess(List(), restInput)
            }

          case ParseFailure(_, restInput) =>
            ParseSuccess(List(), restInput)
        }
    }
  }

  def +(): Parser[List[T]] =
    this ~ (this *) ^^ {
      case (x, xs) => x :: xs
    }

  def ^^[U](f: T => U): Parser[U] = {
    val p = this
    new Parser[U] {
      override def apply(input: String) =
        p(input) match {
          case ParseSuccess(value, restInput) =>
            ParseSuccess(f(value), restInput)

          case ParseFailure(message, restInput) =>
            ParseFailure(message, restInput)
        }
    }
  }
}
