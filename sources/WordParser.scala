
object WordParser extends Parser[String] {
  override def apply(input: String): ParseResult[String] = 
    input.dropWhile(_.isSpaceChar).span(_.isLetter) match {
      case (word, restInput) if !word.isEmpty =>
        ParseSuccess(word, restInput)

      case (_, restInput) =>
        if(restInput.isEmpty) {
          ParseFailure(
            s"Letter expected but nothing found",
            restInput)
        } else {
          ParseFailure(
            s"Letter expected but `${restInput(0)}' found",
            restInput)
        }
    }
}
