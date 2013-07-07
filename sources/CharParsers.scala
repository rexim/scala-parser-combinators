import scala.language.implicitConversions
import scala.language.postfixOps

/** CharParsers is a component that ''provides'' char-level parsers.
  */
trait CharParsers {

  /** A parser for a single char.
    *
    * @param x a char to parse.
    *
    * @return a `Parser` that succeeds on `x` and fails otherwise.
    */
  def elem(x: Char) = new Parser[Char] {
    override def apply(input: String) = {
      if(input.isEmpty) {
        Failure(s"`$x' expected but found nothing", input)
      } else if(input.head == x) {
        Success(x, input.tail)
      } else {
        Failure(s"`$x' expected but `${input.head}' found", input)
      }
    }
  }

  /** A parser for a single symbol.
    *
    * Unlike [[CharParsers.elem]] ignores leading spaces.
    *
    * @param x a symbol to parse. Any character is allowed except space.
    *
    * @return a `Parser` that succeeds on `x` ignoring leading spaces
    *         and fails otherwise
    */
  implicit def symbol(x: Char): Parser[Char] =
    if(x != ' ') {
      rep(elem(' ')) ~> elem(x)
    } else {
      sys.error("symbol: x is space")
    }

  /** A parser for one of symbols from a given string `xs`.
    *
    * Like [[CharParsers.symbol]] ignores leading spaces.
    *
    * @param xs a set of symbols to parse. Any character is allowed except
    *           space.
    *
    * @return a `Parser` that succeeds on any symbol from `xs` ignoring
    *         leading spaces and fails otherwise.
    */
  implicit def oneOfSymbols(xs: String): Parser[Char] =
    if(!xs.isEmpty) {
      val p = xs.tail.foldLeft(elem(xs.head)) {
        case (acc, x) => acc | elem(x)
      }
      rep(elem(' ')) ~> p
    } else {
      sys.error("oneOfSymbols: string is empty")
    }

  /** A parser generator for repetitions.
    *
    * Added to simplify porting of [[Calculator]] example.
    *
    * @param p a `Parser` that is to be applied successively to the input.
    *
    * @return `p.*`
    */
  def rep[T](p: Parser[T]): Parser[List[T]] = p *
}
