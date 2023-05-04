package interpreter

import cats.{InjectK, MonadError, ~>}
import cats.free.Free
import cats.data.EitherK
import cats.free.Free.{liftF, liftInject}
import cats.implicits.*
import cats.mtl.{Ask, Local, Stateful}
import cats.syntax.*
import com.sun.org.apache.xpath.internal.operations.Minus

import java.io.{BufferedReader, PrintWriter}
import scala.annotation.tailrec


case class PrimitiveFunc(name: String, f: List[Scheme] => PrimitivesAlg[Scheme]):
  def apply(args: List[Scheme]): PrimitivesAlg[Scheme] = f(args)

enum PortHandler:
  case WriterPort(writer: PrintWriter)
  case ReaderPort(reader: BufferedReader)

enum Scheme:
  case Atom(value: String)
  case Number(value: Int)
  case Str(value: String)
  case Bool(value: Boolean)
  case Character(value: Char)
  case Vector(value: Array[Scheme])
  case SList(list: List[Scheme])
  case DottedList(head: List[Scheme], tail: Scheme)
  case PrimitiveOperator(func: PrimitiveFunc)
  case Func(params: List[String], body: List[Scheme], closure: Context, vararg: Option[String])
  case Port(port: PortHandler)


def mkString(scheme: Scheme): String =
  scheme match
    case Scheme.Atom(value) => value
    case Scheme.SList(list) => "(" + list.map(mkString).mkString(" ") + ")"
    case Scheme.DottedList(head, tail) => "(" + head.map(mkString).mkString(" ") + " . " + tail + ")"
    case Scheme.Vector(l) => "#(" + l.map(mkString).mkString(" ") + ")"
    case Scheme.Number(value) => value.toString
    case Scheme.Str(value) => "\"" + value + "\""
    case Scheme.Bool(value) => if value then "#t" else "#f"
    case Scheme.Character(value) => value.toString
    case Scheme.Func(params, body, _, None) => "(lambda (" + params.mkString(" ") + ") (" + body.map(mkString).mkString(" ") + "))"
    case Scheme.Func(params, body, _, Some(vararg)) => "(lambda (" + params.mkString(" ") + " . " + vararg + ") (" + body.map(mkString).mkString(" ") + "))"
    case Scheme.PrimitiveOperator(PrimitiveFunc(name, _)) => name
    case Scheme.Port(_) => "<port>"


enum RuntimeError:
  case ArgsCount(expected: Integer, found: List[Scheme])
  case TypeMismatch(expected: String, found: Scheme)
  case BadSpecialForm(message: String, expr: Scheme)
  case NotFunction(message: String, funcId: String)
  case UnboundVar(message: String, id: String)
  case ParseError(message: String)


enum InterpreterAlg[A]:
  case Canonical(x: Scheme) extends InterpreterAlg[Scheme]
  case Variable(name: String) extends InterpreterAlg[Scheme]
  case Binding(name: String, x: Scheme) extends InterpreterAlg[Unit]
  case UpdateBinding(name: String, x: Scheme) extends InterpreterAlg[Unit]
  case Context extends InterpreterAlg[Context]
  case ContextShift(newContext: Context) extends InterpreterAlg[Unit]

enum ErrorAlg[A]:
  case Raise[B](error: RuntimeError) extends ErrorAlg[B]


private object Interpreter:
  def canonical[F[_]](x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Canonical(x))

  def variable[F[_]](name: String)(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Variable(name))

  def binding[F[_]](name: String, x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Unit] =
    liftInject(InterpreterAlg.Binding(name, x))

  def updateBinding[F[_]](name: String, x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Unit] =
    liftInject(InterpreterAlg.UpdateBinding(name, x))

  def context[F[_]](using InjectK[InterpreterAlg, F]): Free[F, Context] =
    liftInject(InterpreterAlg.Context)

  def contextShift[F[_]](newContext: Context)(using InjectK[InterpreterAlg, F]): Free[F, Unit] =
    liftInject(InterpreterAlg.ContextShift(newContext))

object Error:
  def raise[F[_], A](error: RuntimeError)(using InjectK[ErrorAlg, F]): Free[F, A] =
    liftInject(ErrorAlg.Raise[A](error))

  def raiseWhen[F[_]](cond: => Boolean)(error: RuntimeError)(using InjectK[ErrorAlg, F]): Free[F, Unit] =
    if cond then
      liftInject(ErrorAlg.Raise[Unit](error))
    else
      Free.pure[F, Unit](())

private def applyFunc[F[_]](func: Scheme, args: List[Scheme])(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F])(using InjectK[PrimitivesAlg, F]): Free[F, Scheme] =
  def checkArgsCount(params: List[String], vararg: Option[String], args: List[Scheme]) =
    (params.length + (if vararg.isDefined then 1 else 0) > args.length) || (vararg.isEmpty && args.length != params.length)

  func match
    case Scheme.PrimitiveOperator(func: PrimitiveFunc) => Free.liftInject(func(args))
    case Scheme.Func(params, body, closure, vararg) =>
      for
        _ <- Error.raiseWhen(checkArgsCount(params, vararg, args))(RuntimeError.ArgsCount(params.length, args))
        varargArgument = vararg match
          case Some(name) => List((name, Scheme.SList(args.drop(params.length))))
          case None => Nil
        oldContext <- Interpreter.context
        lambdaContext = closure.provide(params.zip(args) ++ varargArgument)
        bodyResult <- Interpreter.contextShift(lambdaContext) *> body.traverse(interpret) <* Interpreter.contextShift(oldContext)
      yield bodyResult.last
    case _ => Error.raise(RuntimeError.NotFunction("Primitive not found", mkString(func)))


def interpret[F[_]](scheme: Scheme)(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F])(using InjectK[PrimitivesAlg, F]): Free[F, Scheme] =
  scheme match
    case s@Scheme.Number(_) => Interpreter.canonical(s)
    case s@Scheme.Bool(_) => Interpreter.canonical(s)
    case s@Scheme.Str(_) => Interpreter.canonical(s)
    case s@Scheme.Character(_) => Interpreter.canonical(s)
    case s@Scheme.Vector(_) => Interpreter.canonical(s)
    case Scheme.SList(List(Scheme.Atom("quote"), data)) => Interpreter.canonical(data)
    case Scheme.SList(List(Scheme.Atom("if"), pred, th, el)) =>
      for
        evalPred <- interpret(pred)
        b <- evalPred match
          case Scheme.Bool(value) => value.pure[Free[F, _]]
          case _ => Error.raise(RuntimeError.TypeMismatch("bool", pred))
        result <- if b then interpret(th) else interpret(el)
      yield result
    case Scheme.Atom(id) => Interpreter.variable(id)
    case Scheme.SList(List(Scheme.Atom("define"), Scheme.Atom(id), value)) =>
      for
        newValue <- interpret(value)
        _ <- Interpreter.binding(id, newValue)
      yield newValue
    case Scheme.SList(Scheme.Atom("define") :: Scheme.SList(Scheme.Atom(id) :: params) :: body) =>
      for
        context <- Interpreter.context
        func = Scheme.Func(params.map(mkString), body, context, None)
        _ <- Interpreter.binding(id, func)
      yield func
    case Scheme.SList(Scheme.Atom("define") :: Scheme.DottedList(Scheme.Atom(id) :: params, vararg) :: body) =>
      for
        context <- Interpreter.context
        func = Scheme.Func(params.map(mkString), body, context, Some(mkString(vararg)))
        _ <- Interpreter.binding(id, func)
      yield func
    case Scheme.SList(Scheme.Atom("lambda") :: Scheme.SList(params) :: body) =>
      for
        context <- Interpreter.context
      yield Scheme.Func(params.map(mkString), body, context, None)

    case Scheme.SList(Scheme.Atom("lambda") :: Scheme.DottedList(params, vararg) :: body) =>
      for
        context <- Interpreter.context
      yield Scheme.Func(params.map(mkString), body, context, Some(mkString(vararg)))

    case Scheme.SList(List(Scheme.Atom("set!"), Scheme.Atom(id), value)) =>
      for
        newValue <- interpret(value)
        _ <- Interpreter.updateBinding(id, newValue)
      yield newValue

    case Scheme.SList(func :: args) =>
      for
        func <- interpret(func)
        args <- args.traverse(interpret)
        apply <- applyFunc(func, args)
      yield apply
    case s@_ => Error.raise(RuntimeError.BadSpecialForm("No progress", s))

def interpreterCompiler[M[_]](using s: Stateful[M, Context])(using me: MonadError[M, RuntimeError]): InterpreterAlg ~> M =

  new(~>[InterpreterAlg, M]):
    def apply[A](alg: InterpreterAlg[A]): M[A] =
      alg match
        case InterpreterAlg.Canonical(x: Scheme) => x.pure[M]
        case InterpreterAlg.Variable(name: String) =>
          for
            ctx <- s.get
            result <- ctx.get(name)
          yield result
        case InterpreterAlg.Binding(name: String, x: Scheme) =>
          for
            _ <- s.modify(_.define(name, x))
          yield ()
        case InterpreterAlg.UpdateBinding(name: String, x: Scheme) =>
          for
            ctx <- s.get
            newCtx <- ctx.set(name, x)
            _ <- s.set(newCtx)
          yield ()
        case InterpreterAlg.Context =>
          for
            ctx <- s.get
          yield ctx
        case InterpreterAlg.ContextShift(newContext) =>
          s.set(newContext)


def errorCompiler[M[_]](using me: MonadError[M, RuntimeError]): ErrorAlg ~> M =
  new(~>[ErrorAlg, M]):
    def apply[A](alg: ErrorAlg[A]): M[A] =
      alg match
        case ErrorAlg.Raise(error) => me.raiseError(error)

def schemeCompiler[M[_]](using s: Stateful[M, Context])(using me: MonadError[M, RuntimeError]) =
  interpreterCompiler[M] or (errorCompiler[M] or primitivesCompiler[M])