import cats.implicits.*
import cats.syntax.*
import cats.{InjectK, MonadError, ~>}
import cats.data.{EitherK, StateT}
import cats.free.Free
import interpreter.{Context, ErrorAlg, InterpreterAlg, Primitives, PrimitivesAlg, RuntimeError, Scheme, interpret, mkString, schemeCompiler}

import scala.io.{Source, StdIn}

enum ReplAlg[A]:
  case ReadModule(name: String) extends ReplAlg[List[Scheme]]
  case ReadLine() extends ReplAlg[Scheme]
  case PrintResult(result: Scheme) extends ReplAlg[Unit]

object Repl:
  def readModule[F[_]](name: String)(using InjectK[ReplAlg, F]): Free[F, List[Scheme]] =
    Free.liftInject(ReplAlg.ReadModule(name))

  def readLine[F[_]](using InjectK[ReplAlg, F]): Free[F, Scheme] =
    Free.liftInject(ReplAlg.ReadLine())

  def printResult[F[_]](result: Scheme)(using InjectK[ReplAlg, F]): Free[F, Unit] =
    Free.liftInject(ReplAlg.PrintResult(result))

def replCompiler[M[_]](using me: MonadError[M, RuntimeError]): ReplAlg ~> M =
  new(ReplAlg ~> M):
    override def apply[A](fa: ReplAlg[A]): M[A] =
      fa match
        case ReplAlg.ReadModule(name) =>
          val file = Source.fromFile(name)
          val text = file.mkString
          file.close()
          val parseResult = ASTParser.parseExprSeq(text)
          parseResult match
            case ASTParser.Success(astList, _) =>
              val body = astList.map(astToScheme)
              me.pure(body)
            case ASTParser.Failure(msg, _) =>
              me.raiseError(RuntimeError.ParseError(msg))
            case ASTParser.Error(msg, _) =>
              me.raiseError(RuntimeError.ParseError(msg))

        case ReplAlg.ReadLine() =>
          val text = StdIn.readLine(">>> ")
          val parseResult = ASTParser.parseExpr(text)
          parseResult match
            case ASTParser.Success(ast, _) =>
              val body = astToScheme(ast)
              me.pure(body)
            case ASTParser.Failure(msg, _) =>
              me.raiseError(RuntimeError.ParseError(msg))
            case ASTParser.Error(msg, _) =>
              me.raiseError(RuntimeError.ParseError(msg))

        case ReplAlg.PrintResult(result) =>
          println(mkString(result))
          ().pure[M]


def runRepl[F[_]](using InjectK[ReplAlg, F])(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F])(using InjectK[PrimitivesAlg, F]): Free[F, Unit] =
  for
    newLine <- Repl.readLine
    result <- interpret(newLine)
    _ <- Repl.printResult(result)
    _ <- runRepl
  yield ()

def runModule[F[_]](filename: String)(using InjectK[ReplAlg, F])(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F])(using InjectK[PrimitivesAlg, F]): Free[F, Unit] =
  for
    body <- Repl.readModule(filename)
    result <- body.traverse(interpret)
  yield result.last

def run[F[_]](filename: Option[String], interactive: Boolean)(using InjectK[ReplAlg, F])(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F])(using InjectK[PrimitivesAlg, F]): Free[F, Unit] =
  for
    _ <- filename match
      case Some(module) => runModule(module)
      case _ => ().pure[Free[F, _]]
    _ <- if interactive then
      runRepl
    else
      ().pure[Free[F, _]]
  yield ()


type ExecutionResult[A] = Either[RuntimeError, A]

def rawExecuteProgram[M[_]](input: String)(using me: MonadError[M, RuntimeError]): M[Scheme] =
  type Algebra[A] = EitherK[InterpreterAlg, EitherK[ErrorAlg, PrimitivesAlg, _], A]

  val parseResult = ASTParser.parseExprSeq(input)
  parseResult match
    case ASTParser.Success(astList, _) =>
      val body = astList.map(astToScheme)
      val semantic = body.traverse(interpret[Algebra]).foldMap(schemeCompiler[StateT[M, Context, _]])
      semantic.runA(Context.withPrimitives[StateT[M, Context, _]]).map(_.last)
    case ASTParser.Failure(msg, _) =>
      me.raiseError(RuntimeError.ParseError(msg))
    case ASTParser.Error(msg, _) =>
      me.raiseError(RuntimeError.ParseError(msg))
