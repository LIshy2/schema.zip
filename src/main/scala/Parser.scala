
import interpreter.Scheme
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

enum AST:
  case Atom(value: String)
  case SList(list: List[AST])
  case DottedList(head: List[AST], tail: AST)
  case Vector(data: List[AST])
  case Number(value: Int)
  case Str(value: String)
  case Bool(value: Boolean)
  case Character(value: Char)


object ASTParser extends RegexParsers:

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  private def stringParser: Parser[AST] = "\"((?!\")[ -~])*\"".r ^^ (s => AST.Str(s.drop(1).dropRight(1)))

  private def numberParser: Parser[AST] = "(\\d)+".r ^^ (s => AST.Number(s.toInt))

  private def boolParser: Parser[AST] = "(#t|#f)".r ^^ {
    case "#t" => AST.Bool(true)
    case "#f" => AST.Bool(false)
  }

  private def charParser: Parser[AST] = "#\\.".r ^^ {
    s => AST.Character(s.last)
  }

  private val letters = "[a-z]".r
  private val specialInitial = "[!$%&|*/:<=>?^_~]".r
  private val digit = "\\d".r
  private val initial = s"$letters|$specialInitial".r
  private val specialSubsequent = "[+-.@]".r
  private val subsequent = s"($initial)|$digit|$specialSubsequent".r
  private val peculiarIdentifier = "\\+|-|\\.\\.\\.".r

  private def atomParser: Parser[AST] = s"(($initial)($subsequent)*)|($peculiarIdentifier)".r ^^ (a => AST.Atom(a))

  private def listParser: Parser[AST] =
    for
      list <- rep(exprParser)
    yield AST.SList(list)

  private def dottedParser: Parser[AST] =
    for
      head <- rep1(exprParser)
      _ <- "." ^^ identity
      tail <- exprParser
    yield AST.DottedList(head, tail)

  private def quoteParser: Parser[AST] =
    for
      _ <- "'" ^^ identity
      expr <- exprParser
    yield AST.SList(List(AST.Atom("quote"), expr))

  private def vectorParser: Parser[AST] =
    for
      _ <- "#(" ^^ identity
      data <- rep(exprParser)
      _ <- ")" ^^ identity
    yield AST.Vector(data)


  private def exprParser: Parser[AST] =
    val parseList = for
      _ <- "(" ^^ identity
      data <- dottedParser | listParser
      _ <- ")" ^^ identity
    yield data
    boolParser | numberParser | stringParser | charParser | atomParser | quoteParser | vectorParser | parseList


  def parseExpr(data: String): ParseResult[AST] =
    parse(exprParser, data)

  def parseExprSeq(data: String): ParseResult[List[AST]] =
    parse(rep1(exprParser), data)


def astToScheme(ast: AST): Scheme =
  ast match
    case AST.Atom(value) => Scheme.Atom(value)
    case AST.SList(list) => Scheme.SList(list.map(astToScheme))
    case AST.DottedList(head, tail) => Scheme.DottedList(head.map(astToScheme), astToScheme(tail))
    case AST.Number(value) => Scheme.Number(value)
    case AST.Str(value) => Scheme.Str(value)
    case AST.Bool(value) => Scheme.Bool(value)
    case AST.Character(value) => Scheme.Character(value)
    case AST.Vector(value) => Scheme.Vector(value.map(astToScheme).toArray)
