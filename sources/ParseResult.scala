
trait ParseResult[T]
case class Success[T](value: T, input: String) extends ParseResult[T]
case class Failure[T](message: String, input: String) extends ParseResult[T]
