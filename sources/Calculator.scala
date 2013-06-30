// This example was taken and ported from here:
// http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

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

  def apply(input: String) =
    expr(input) match {
      case ParseSuccess(result, _) => println(s"=> $result")
      case ParseFailure(message, _) => println(s"Error: $message")
    }
}
