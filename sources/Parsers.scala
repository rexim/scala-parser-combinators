import scala.language.implicitConversions
import scala.language.postfixOps

trait CharParsers {
  implicit def Char2CharParser(x: Char): Parser[Char] = new Parser[Char] {
    override def apply(input: String) = {
      val trimmedInput = input.dropWhile(_.isSpaceChar)
      if(trimmedInput.isEmpty) {
        ParseFailure(s"`$x' expected but found nothing", trimmedInput)
      } else if(trimmedInput.head == x) {
        ParseSuccess(x, trimmedInput.tail)
      } else {
        ParseFailure(s"`$x' expected but `${trimmedInput.head}' found", trimmedInput)
      }
    }
  }

  implicit def String2CharParser(xs: String): Parser[Char] =
    if(!xs.isEmpty) {
      xs.tail.foldLeft(xs.head: Parser[Char]) {
        case (acc, x) => acc | x
      }
    } else {
      sys.error("String2CharParser: string is empty")
    }

  def rep[T](p: Parser[T]): Parser[List[T]] = p *
}
