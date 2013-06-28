
object IntParser extends Parser[Int] {
  override def apply(input: String): ParseResult[Int] =
    input.dropWhile(_.isSpaceChar).span(_.isDigit) match {
      case (number, restInput) if !number.isEmpty =>
        ParseSuccess[Int](number.toInt, restInput)

      case (_, restInput) =>
        if(restInput.isEmpty) {
          ParseFailure[Int](
            s"Digit expected but nothing found",
            restInput)
        } else {
          ParseFailure[Int](
            s"Digit expected but `${restInput(0)}' found",
            restInput)
        }
    }
}
