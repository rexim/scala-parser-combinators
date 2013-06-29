import scala.language.implicitConversions
import scala.language.postfixOps

trait CharParsers {
  implicit def Char2CharParser(x: Char): Parser[Char] = new Parser[Char] {
    override def apply(input: String) =
      if(input.dropWhile(_.isSpaceChar).isEmpty) {
        ParseFailure(s"`$x' expected but found nothing", input)
      } else if(input.head == x) {
        ParseSuccess(x, input.tail)
      } else {
        ParseFailure(s"`$x' expected but `${input.head}' found", input)
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

// FIXME(rexim): this is a ported example from here:
// http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.RegexParsers
// It is not working yet.
object Calculator extends CharParsers {
  def digit: Parser[Char] = "0123456789"

  def number: Parser[Int] = (digit +) ^^ { _.mkString.toInt }
  def factor: Parser[Int] = number | '(' ~> expr <~ ')'
  def term: Parser[Int] = factor ~ rep('*' ~ factor | '/' ~ factor) ^^ {
    case (number, list) => list.foldLeft(number) {
      case (x, ('*', y)) => x * y
      case (x, ('/', y)) => x / y
    }
  }
  def expr: Parser[Int] = term ~ rep('+' ~ term | '-' ~ term) ^^ {
    case (number, list) => list.foldLeft(number) {
      case (x, ('+', y)) => x + y
      case (x, ('-', y)) => x - y
    }
  }
}
