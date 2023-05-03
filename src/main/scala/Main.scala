import cats.{InjectK, Monad, MonadError}
import cats.data.{EitherK, StateT}
import cats.free.Free
import cats.free.Free.liftInject
import cats.syntax.*
import cats.mtl.implicits.*
import cats.implicits.*
import scopt.OParser

import java.io.FileReader
import interpreter.{Context, ErrorAlg, ExecutionAlg, Interpreter, InterpreterAlg, Primitives, RuntimeError, Scheme, compiler, interpret, primitives, numOperator, eqNumOperator, eqStrOperators, equalsOperators, listOperators, boolOperator, mkString}

import scala.io.Source


case class Config(filename: Option[String], repl: Boolean)


def buildParser(): OParser[Unit, Config] =
  val builder = OParser.builder[Config]
  OParser.sequence(
    builder.programName("schema.zip"),
    builder.opt[Unit]("i").action((_, c) => c.copy(repl = true)),
    builder.opt[Option[String]]("file").optional().action((x, c) => c.copy(filename = x))
  )

def showError(error: RuntimeError): Unit =
  error match
    case RuntimeError.ArgsCount(expected: Integer, found: List[Scheme]) => println(s"Wrong arguments.\nExpected $expected, found ${found.map(mkString).mkString(" ")}")
    case RuntimeError.TypeMismatch(expected: String, found: Scheme) => println(s"Type mismatch.\nExpected $expected, found ${mkString(found)}")
    case RuntimeError.BadSpecialForm(message: String, expr: Scheme) => println(s"Bad Form.\nMessage: $message\nExpr: ${mkString(expr)}")
    case RuntimeError.NotFunction(message: String, funcId: String) => println(s"Not function.\nMessage: $message\nFunc: $funcId")
    case RuntimeError.UnboundVar(message: String, id: String) => println(s"Unbound variable.\nMessage: $message\nId: $id")
    case RuntimeError.ParseError(message: String) => println(s"Parse error.\nMessage: $message")


def execute(config: Config): ExecutionResult[Unit] =
  type StateExecutionResult[A] = StateT[ExecutionResult, Context, A]
  type Algebra[A] = EitherK[ReplAlg, EitherK[InterpreterAlg, ErrorAlg, _], A]
  run[Algebra](config.filename, config.repl).foldMap(replCompiler[StateExecutionResult] or compiler[StateExecutionResult]).runA(Context.withPrimitives[StateExecutionResult])

@main
def main(args: String*): Unit = {
  val parser = buildParser()
  OParser.parse(parser, args, Config(None, false)) match {
    case Some(config) =>
      val result = execute(config)
      result match
        case Left(error) => showError(error)
        case _ => ()
    case None => throw IllegalArgumentException("Wrong CLI arguments")
  }
}