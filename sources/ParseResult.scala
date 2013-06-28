
trait ParseResult[T]
case class ParseSuccess[T](value: T, input: String) extends ParseResult[T]
case class ParseFailure[T](message: String, input: String) extends ParseResult[T]
