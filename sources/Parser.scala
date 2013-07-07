import scala.language.postfixOps

/** A parsers trait. Parsers are functions from the String type to
  * ParseResult.
  */
trait Parser[+T] extends (String => ParseResult[T]) {
  /** An unspecified method that defines the behaviour of this parser.
    *
    * @return the result of function application.
    */
  def apply(input: String): ParseResult[T]

  /** A parser combinator for sequential composition.
    *
    * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input
    * left over by `p`
    *
    * @param q a parser that will be executed after `p` (this parser)
    *          succeeds -- evaliated at most once, and only when
    *          necessary.
    * 
    * @return a `Parser` that -- on success -- returns a pair that
    *         contains the result of `p` and that of `q`. The
    *         resulting parser fails if either `p` or `q` fails.
    */
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

  /** A parser combinator for sequential composition which keeps only
    * the right result.
    * 
    * `p ~> q` succeed if `p` succeeds and `q` succeeds on the input
    * left over by `p`.
    * 
    * @param q a parser that will be executed after p (this parser)
    *          succeeds -- evaluated at most once, nad only when
    *          neccessary.
    * 
    * @return a `Parser` that -- on success -- returns the result of
    *         `q`.
    */
  def ~>[U](q: => Parser[U]): Parser[U] =
    this ~ q ^^ {
      case (_, result) => result
    }

  /** A parser combinator for sequential composition which keeps only
    * the left result.
    *
    * `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input
    * left over by `p`.
    *
    * @param q a parser that will be executed after `p` (this parser)
    *          succeeds -- evaluated at most once, and only when
    *          necessary.
    * 
    * @return a `Parser` that -- on success -- returns the result of
    *         `p`.
    */
  def <~[U](q: => Parser[U]): Parser[T] =
    this ~ q ^^ {
      case (result, _) => result
    }

  /** A parser combinator for alternative composition.
    *
    * `p | q` succeeds if `p` succeeds or `q` succeeds. Note that `q`
    * is only tried if `p`s fails (i.e., back-tracking is allowed).
    *
    * @param q a parser that will be executed if p (this parser) fails
    *          (and allows back-tracking).
    * 
    * @return a `Parser` that returns the result of the first parser
    *         to succeed (out of `p` and `q`). The resulting parser
    *         succeeds if (and only if)
    *         - p succeeds, ''or''
    *         - if `p` fails allowing back-tacking and `q` succeeds.
    */
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
  /** A parser combinator for optional sub-phrases.
    *
    * The result parser returns `Some(x)`, if `p` (this parser)
    * returns `x` and `None` if `p` fails.
    *
    * @return a `Parser` that always succeeds: either with the result
    *         provided by p or with the empty result.
    */
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

  /** A parser combinator for repetitions.
    *
    * The result parser repeatedly uses `p` (this parser) to parse the
    * input until `p` fails (the result is a `List` of the consecutive
    * results of `p`)
    *
    * @return a `Parser` that returns a list of results produced by
    *         repeatedly applying `p` to the input.
    */
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

  /** A parser combinator for non-empty repetitions.
    *
    * The result parser repeatedly uses `p` (this parser) to parse the
    * input until `p` fails -- `p` must succeed at least once (the
    * result is a `List` of the consecutive results of `p`)
    *
    * @return a `Parser` that returns a list of results produced by
    *         repeatedly applying `p` to the input (and that only
    *         succeeds if `p` matches at least once).
    */
  def +(): Parser[List[T]] =
    this ~ (this *) ^^ {
      case (x, xs) => x :: xs
    }

  /** A parser combinator for function application
    *
    * `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the
    * result of p.
    *
    * @param f a function that will be applied to this parser's result
    *
    * @return a `Parser` that has the same behaviour as the current parser,
              but whose result is transformed by `f`.
    */
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
