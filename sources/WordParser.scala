
object WordParser extends Parser[String] {
  override def apply(input: String): ParseResult[String] = 
    input.dropWhile(_.isSpaceChar).span(_.isLetter) match {
      case (word, restInput) if !word.isEmpty =>
        Success(word, restInput)
      case _ =>
        Failure("fail", input)
    }
}
