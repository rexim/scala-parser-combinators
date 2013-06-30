import scala.language.postfixOps

trait Parser[+T] extends (String => ParseResult[T]) {
  def ~[U](q: => Parser[U]): Parser[(T, U)] = {
    val p = this
    new Parser[(T, U)] {
      override def apply(input: String) =
        p(input) match {
          case Success(value1, nextInput) =>
            q(nextInput) match {
              case Success(value2, restInput) =>
                Success((value1, value2), restInput)

              case Failure(errorMessage, restInput) =>
                Failure(errorMessage, restInput)
            }

          case Failure(errorMessage, restInput) =>
            Failure(errorMessage, restInput)
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
          case Success(value, restInput) => Success(value, restInput)
          case Failure(_, _) => q(input)
        }
    }
  }

  def ?(): Parser[Option[T]] = {
    val p = this
    new Parser[Option[T]] {
      override def apply(input: String) =
        p(input) match {
          case Success(value, restInput) =>
            Success(Some(value), restInput)

          case Failure(_, restInput) =>
            Success(None, restInput)
        }
    }
  }

  def *(): Parser[List[T]] = {
    val p = this
    new Parser[List[T]] {
      override def apply(input: String) =
        p(input) match {
          case Success(value, nextInput) =>
            apply(nextInput) match {
              case Success(values, restInput) =>
                Success(value :: values, restInput)

              case Failure(_, restInput) =>
                Success(List(), restInput)
            }

          case Failure(_, restInput) =>
            Success(List(), restInput)
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
          case Success(value, restInput) =>
            Success(f(value), restInput)

          case Failure(message, restInput) =>
            Failure(message, restInput)
        }
    }
  }
}
