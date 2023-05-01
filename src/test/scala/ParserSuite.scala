import AST.DottedList

class ParserSuite extends munit.FunSuite {
  def assertParser[A](parseResult: ASTParser.ParseResult[A], expected: A): Unit =
    parseResult match
      case ASTParser.Success(result, _) => assertEquals(result, expected)
      case ASTParser.Failure(msg, _) => fail(msg)
      case ASTParser.Error(msg, _) => fail(msg)

  test("Number Parsing") {
    assertParser(ASTParser.parseExpr("1"), AST.Number(1))
  }
  test("String Parsing") {
    assertParser(ASTParser.parseExpr("\"ab'123   123_'c\""), AST.Str("ab'123   123_'c"))
  }
  test("Boolean Parsing") {
    assertParser(ASTParser.parseExpr("#t"), AST.Bool(true))
    assertParser(ASTParser.parseExpr("#f"), AST.Bool(false))
  }
  test("Atom Parsing") {
    assertParser(ASTParser.parseExpr("id"), AST.Atom("id"))
  }
  test("List Atom Parsing") {
    val astResult = AST.SList(List(AST.Atom("a"), AST.Atom("b"), AST.Atom("c")))
    assertParser(ASTParser.parseExpr("(a b c)"), astResult)
  }
  test("DottedList Atom Parsing") {
    val astResult = AST.DottedList(List(AST.Atom("a"), AST.Atom("b")), AST.Atom("c"))
    assertParser(ASTParser.parseExpr("(a b . c)"), astResult)
  }
  test("Recursive List Parsing") {
    val astResult = AST.SList(List(
      AST.Atom("a"),
      AST.Number(3),
      AST.SList(List(
        AST.Number(1),
        AST.Number(2),
        AST.DottedList(
          List(
            AST.Number(1),
            AST.Bool(true),
          ),
          AST.Number(3)
        )
      ))))
    assertParser(ASTParser.parseExpr("(a 3 (1 2 (1 #t . 3)))"), astResult)
  }

}