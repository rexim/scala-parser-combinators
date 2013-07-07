
/** A trait for parser results. A result is either successful or not.
  */
trait ParseResult[+T]

/** The success case of `ParseResult`: contains the result and the
  * remaining input.
  */
case class Success[+T](value: T, input: String) extends ParseResult[T]

/** The failure case of `ParseResult`: contains an error-message and
  * the remaining input.
  */
case class Failure[+T](message: String, input: String) extends ParseResult[T]
