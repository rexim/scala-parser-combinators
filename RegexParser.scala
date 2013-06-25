import scala.util.matching.Regex

class RegexParser(r: Regex) extends Parser[Regex.Match] {
  override def apply(input: String) = {
    val source = input.dropWhile(_.isSpaceChar)
    r.findPrefixMatchOf(source) match {
      case Some(matched) =>
        Success(matched, source.drop(matched.end))
      case None =>
        Failure("fail", input)
    }
  }
}
