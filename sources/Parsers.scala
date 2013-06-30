import scala.language.implicitConversions
import scala.language.postfixOps

trait CharParsers {
  implicit def OneChar(x: Char): Parser[Char] =
    rep(elem(' ')) ~> elem(x)

  implicit def OneOfChars(xs: String): Parser[Char] =
    if(!xs.isEmpty) {
      val p = xs.tail.foldLeft(elem(xs.head)) {
        case (acc, x) => acc | elem(x)
      }
      rep(elem(' ')) ~> p
    } else {
      sys.error("OneOfChars: string is empty")
    }

  def rep[T](p: Parser[T]): Parser[List[T]] = p *

  def elem(x: Char) = new Parser[Char] {
    override def apply(input: String) = {
      if(input.isEmpty) {
        ParseFailure(s"`$x' expected but found nothing", input)
      } else if(input.head == x) {
        ParseSuccess(x, input.tail)
      } else {
        ParseFailure(s"`$x' expected but `${input.head}' found", input)
      }
    }
  }
}
