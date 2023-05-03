package interpreter

import cats.{InjectK, MonadError, ~>}
import cats.free.Free
import cats.data.EitherK
import cats.free.Free.{liftF, liftInject}
import cats.implicits.*
import cats.mtl.{Ask, Local, Stateful}
import cats.syntax.*
import com.sun.org.apache.xpath.internal.operations.Minus
import interpreter.RuntimeError.TypeMismatch

import scala.annotation.tailrec


case class PrimitiveFunc[M[_]](name: String, f: List[Scheme] => M[Scheme]):
  def apply(args: List[Scheme]): M[Scheme] = f(args)

enum Scheme:
  case Atom(value: String)
  case Number(value: Int)
  case Str(value: String)
  case Bool(value: Boolean)
  case Character(value: Char)
  case Vector(value: Array[Scheme])
  case SList(list: List[Scheme])
  case DottedList(head: List[Scheme], tail: Scheme)
  case PrimitiveOperator[M[_]](func: PrimitiveFunc[M])
  case Func(params: List[String], body: List[Scheme], closure: Context, vararg: Option[String])


def show(scheme: Scheme): String =
  scheme match
    case Scheme.Atom(value) => value
    case Scheme.SList(list) => "(" + list.map(show).mkString(" ") + ")"
    case Scheme.DottedList(head, tail) => "(" + head.map(show).mkString(" ") + " . " + tail + ")"
    case Scheme.Number(value) => value.toString
    case Scheme.Str(value) => "\"" + value + "\""
    case Scheme.Bool(value) => if value then "#t" else "#f"
    case Scheme.Func(params, body, _, None) => "(lambda (" + params.mkString(" ") + ") (" + body.map(show).mkString(" ") + "))"
    case Scheme.Func(params, body, _, Some(vararg)) => "(lambda (" + params.mkString(" ") + " . " + vararg + ") (" + body.map(show).mkString(" ") + "))"
    case Scheme.PrimitiveOperator(PrimitiveFunc(name, _)) => name


def checkArgsCount(params: List[String], vararg: Option[String], args: List[Scheme]) =
  (params.length + (if vararg.isDefined then 1 else 0) > args.length) || (vararg.isEmpty && args.length != params.length)


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
  case Lambda(params: List[String], body: List[Scheme], vararg: Option[String]) extends InterpreterAlg[Scheme]
  case Call(func: Scheme, args: List[Scheme]) extends InterpreterAlg[Scheme]

enum ErrorAlg[A]:
  case Raise[B](error: RuntimeError) extends ErrorAlg[B]

type ExecutionAlg[A] = EitherK[InterpreterAlg, ErrorAlg, A]
type Execution[A] = Free[ExecutionAlg, A]

object Interpreter:
  def canonical[F[_]](x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Canonical(x))

  def variable[F[_]](name: String)(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Variable(name))

  def binding[F[_]](name: String, x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Unit] =
    liftInject(InterpreterAlg.Binding(name, x))

  def updateBinding[F[_]](name: String, x: Scheme)(using InjectK[InterpreterAlg, F]): Free[F, Unit] =
    liftInject(InterpreterAlg.UpdateBinding(name, x))

  def lambda[F[_]](params: List[String], body: List[Scheme], vararg: Option[String])(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Lambda(params, body, vararg))

  def call[F[_]](func: Scheme, args: List[Scheme])(using InjectK[InterpreterAlg, F]): Free[F, Scheme] =
    liftInject(InterpreterAlg.Call(func, args))

object Error:
  def raise[F[_], A](error: RuntimeError)(using InjectK[ErrorAlg, F]): Free[F, A] =
    liftInject(ErrorAlg.Raise[A](error))


def interpret[F[_]](scheme: Scheme)(using InjectK[InterpreterAlg, F])(using InjectK[ErrorAlg, F]): Free[F, Scheme] =
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
        func <- Interpreter.lambda(params.map(show), body, None)
        _ <- Interpreter.binding(id, func)
      yield func
    case Scheme.SList(Scheme.Atom("define") :: Scheme.DottedList(Scheme.Atom(id) :: params, vararg) :: body) =>
      for
        func <- Interpreter.lambda(params.map(show), body, Some(show(vararg)))
        _ <- Interpreter.binding(id, func)
      yield func
    case Scheme.SList(Scheme.Atom("lambda") :: Scheme.SList(params) :: body) =>
      Interpreter.lambda(params.map(show), body, None)
    case Scheme.SList(Scheme.Atom("lambda") :: Scheme.DottedList(params, vararg) :: body) =>
      Interpreter.lambda(params.map(show), body, Some(show(vararg)))
    case Scheme.SList(List(Scheme.Atom("set!"), Scheme.Atom(id), value)) =>
      for
        newValue <- interpret(value)
        _ <- Interpreter.updateBinding(id, newValue)
      yield newValue
    case Scheme.SList(func :: args) =>
      for
        func <- interpret(func)
        args <- args.traverse(interpret)
        apply <- Interpreter.call(func, args)
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
          yield result.asInstanceOf[A]
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
        case InterpreterAlg.Lambda(params: List[String], body: List[Scheme], vararg: Option[String]) =>
          for
            ctx <- s.get
          yield Scheme.Func(params, body, ctx, vararg)
        case InterpreterAlg.Call(func: Scheme, args: List[Scheme]) =>
          func match
            case Scheme.PrimitiveOperator(func: PrimitiveFunc[M]) => func(args)
            case Scheme.Func(params, body, closure, vararg) =>
              for
                _ <- me.raiseWhen(checkArgsCount(params, vararg, args))(RuntimeError.ArgsCount(params.length, args))
                varargArgument = vararg match
                  case Some(name) => List((name, Scheme.SList(args.drop(params.length))))
                  case None => Nil
                oldContext <- s.get
                lambdaContext = closure.provide(params.zip(args) ++ varargArgument)
                bodyResult <- s.set(lambdaContext) *> body.traverse(interpret[ExecutionAlg]).foldMap(compiler[M]) <* s.set(oldContext)
              yield bodyResult.last
            case _ => me.raiseError(RuntimeError.NotFunction("Primitive not found", show(func)))


def errorCompiler[M[_]](using me: MonadError[M, RuntimeError]): ErrorAlg ~> M =
  new(~>[ErrorAlg, M]):
    def apply[A](alg: ErrorAlg[A]): M[A] =
      alg match
        case ErrorAlg.Raise(error) => me.raiseError(error)

def compiler[M[_]](using s: Stateful[M, Context])(using me: MonadError[M, RuntimeError]) =
  interpreterCompiler[M] or errorCompiler[M]