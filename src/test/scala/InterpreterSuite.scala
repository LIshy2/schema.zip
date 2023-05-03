import interpreter.{
  RuntimeError, Scheme, Context,
  primitives,
  numOperator,
  eqNumOperator,
  eqStrOperators,
  equalsOperators,
  boolOperator,
  listOperators
}

class InterpreterSuite extends munit.FunSuite {

  test("Single Arithmetic interpreter") {
    val plusScheme = "(+ 1 2)"
    val minusScheme = "(- 10 2)"
    val divScheme = "(/ 7 2)"
    val mulScheme = "(* 5 2)"
    val modScheme = "(mod 15 4)"
    assertEquals(rawExecuteProgram[ExecutionResult](plusScheme), Right(Scheme.Number(3)))
    assertEquals(rawExecuteProgram[ExecutionResult](minusScheme), Right(Scheme.Number(8)))
    assertEquals(rawExecuteProgram[ExecutionResult](divScheme), Right(Scheme.Number(3)))
    assertEquals(rawExecuteProgram[ExecutionResult](mulScheme), Right(Scheme.Number(10)))
    assertEquals(rawExecuteProgram[ExecutionResult](modScheme), Right(Scheme.Number(3)))
  }
  test("Multiple Arithmetic interpreter") {
    val plusScheme = "(+ 1 2 3 4)"
    val minusScheme = "(- 10 2 3)"

    assertEquals(rawExecuteProgram(plusScheme),
      Right(Scheme.Number(10)))
    assertEquals(rawExecuteProgram(minusScheme),
      Right(Scheme.Number(5)))
  }
  test("Arithmetic Errors interpreter") {
    val typeError = "(+ 1 \"abc\" 3 4)"
    val divZeroError = "(/ 5 0)"
    assertEquals(rawExecuteProgram(typeError), Left(RuntimeError.TypeMismatch("Number", Scheme.Str("abc"))))
    assertEquals(rawExecuteProgram(divZeroError), Left(RuntimeError.BadSpecialForm("Division by Zero", Scheme.Number(0))))
  }
  test("List Operators interpreter") {
    val carScheme = "(car '(1 2 3))"
    val cdrScheme = "(cdr '(1 2 3))"
    val consScheme = "(cons 1 '(2 3))"
    assertEquals(rawExecuteProgram(carScheme), Right(Scheme.Number(1)))
    assertEquals(rawExecuteProgram(cdrScheme), Right(Scheme.SList(List(Scheme.Number(2), Scheme.Number(3)))))
    assertEquals(rawExecuteProgram(consScheme), Right(Scheme.SList(List(Scheme.Number(1), Scheme.Number(2), Scheme.Number(3)))))
  }
  test("Bool Operators interpreter") {
    val orScheme = "(|| #t #f #t #f)"
    val andScheme = "(&& #t #f #t #f)"
    assertEquals(rawExecuteProgram(orScheme), Right(Scheme.Bool(true)))
    assertEquals(rawExecuteProgram(andScheme), Right(Scheme.Bool(false)))
  }
  test("Equals Operators interpreter") {
    val eqScheme = "(= 1 1)"
    val notEqScheme = "(= 4 2)"
    val eqvScheme = "(eqv? '(1 2 3) '(1 2 3))"
    assertEquals(rawExecuteProgram(eqScheme), Right(Scheme.Bool(true)))
    assertEquals(rawExecuteProgram(notEqScheme), Right(Scheme.Bool(false)))
    assertEquals(rawExecuteProgram(eqvScheme), Right(Scheme.Bool(false)))
  }
  test("Context interpreter") {
    val defineScheme =
      """(define x 2)
        (+ x x)
        """
    val setScheme =
      """(define x 2)
          (set! x 3)
          (+ x x)
          """
    val firstScheme =
      """(define first car)
            (first '(1 2))
            """
    assertEquals(rawExecuteProgram(defineScheme), Right(Scheme.Number(4)))
    assertEquals(rawExecuteProgram(setScheme), Right(Scheme.Number(6)))
    assertEquals(rawExecuteProgram(firstScheme), Right(Scheme.Number(1)))
  }

  test("Lambda interpreter") {
    val lambdaScheme = "((lambda (x y z) (+ x y z)) 1 2 3)"
    assertEquals(rawExecuteProgram(lambdaScheme), Right(Scheme.Number(6)))
  }
}
