package interpreter

import cats.syntax.*
import cats.implicits.*
import cats.MonadError

class SchemeRef(var currentValue: Scheme):
  def set(newValue: Scheme): Unit =
    currentValue = newValue

  def get: Scheme = currentValue


class Context(val state: Map[String, SchemeRef]):

  def exists[M[_]](id: String): Boolean = state.contains(id)

  def get[M[_]](id: String)(using me: MonadError[M, RuntimeError]): M[Scheme] = me.fromOption(state.get(id).map(_.get), RuntimeError.UnboundVar("", id))

  def set[M[_]](id: String, value: Scheme)(using me: MonadError[M, RuntimeError]): M[Unit] =
    if this.exists(id) then
      state.get(id).map(_.set(value))
      ().pure[M]
    else me.raiseError(RuntimeError.UnboundVar("Unbounded var in set", id))

  def define(id: String, value: Scheme): Context = new Context(state.updated(id, SchemeRef(value)))

  def provide(newContext: List[(String, Scheme)]): Context =
    newContext.foldLeft(this) {
      case (acc, (id, value)) => acc.define(id, value)
    }

object Context:

  def apply(map: Map[String, Scheme]): Context =
    new Context(map.map((key, value) => (key, SchemeRef(value))))

  def empty[M[_]]: Context = new Context(Map.empty[String, SchemeRef])

  def withPrimitives[M[_]]: Context =
    def wrapPrimitive(name: String, f: List[Scheme] => PrimitivesAlg[Scheme]): Scheme =
      Scheme.PrimitiveOperator(PrimitiveFunc(name, f))

    Context(Map[String, Scheme](
      "+" -> wrapPrimitive("+", PrimitivesAlg.Plus.apply),
      "-" -> wrapPrimitive("+", PrimitivesAlg.Minus.apply),
      "/" -> wrapPrimitive("/", PrimitivesAlg.Div.apply),
      "*" -> wrapPrimitive("*", PrimitivesAlg.Mul.apply),
      "mod" -> wrapPrimitive("mod", PrimitivesAlg.Mod.apply),
      "=" -> wrapPrimitive("=", PrimitivesAlg.NumEq.apply),
      "<" -> wrapPrimitive("<", PrimitivesAlg.NumLess.apply),
      ">" -> wrapPrimitive(">", PrimitivesAlg.NumGreat.apply),
      "/=" -> wrapPrimitive("/=", PrimitivesAlg.NumNotEq.apply),
      "<=" -> wrapPrimitive("<=", PrimitivesAlg.NumLTE.apply),
      ">=" -> wrapPrimitive(">=", PrimitivesAlg.NumGTE.apply),
      "&&" -> wrapPrimitive("&&", PrimitivesAlg.And.apply),
      "||" -> wrapPrimitive("||", PrimitivesAlg.Or.apply),
      "string=?" -> wrapPrimitive("string=?", PrimitivesAlg.StrEq.apply),
      "string<?" -> wrapPrimitive("string<?", PrimitivesAlg.StrLess.apply),
      "string>?" -> wrapPrimitive("string>?", PrimitivesAlg.StrGreat.apply),
      "string<=?" -> wrapPrimitive("string<=?", PrimitivesAlg.StrLTE.apply),
      "string>=?" -> wrapPrimitive("string>=?", PrimitivesAlg.StrGTE.apply),
      "car" -> wrapPrimitive("car", PrimitivesAlg.Car.apply),
      "cdr" -> wrapPrimitive("cdr", PrimitivesAlg.Cdr.apply),
      "cons" -> wrapPrimitive("cons", PrimitivesAlg.Cons.apply),
      "eq?" -> wrapPrimitive("eq?", PrimitivesAlg.DataEq.apply),
      "eqv?" -> wrapPrimitive("eqv?", PrimitivesAlg.DataEqv.apply),
      "make-vector" -> wrapPrimitive("make-vector", PrimitivesAlg.VectorMake.apply),
      "vector-set!" -> wrapPrimitive("vector-set!", PrimitivesAlg.VectorSet.apply),
      "vector-ref" -> wrapPrimitive("vector-ref", PrimitivesAlg.VectorRef.apply),
      "make-vector" -> wrapPrimitive("make-vector", PrimitivesAlg.VectorMake.apply),
      "open-input-file" -> wrapPrimitive("vector-set!", arg => PrimitivesAlg.MakePort(FileMode.Reader, arg)),
      "open-output-file" -> wrapPrimitive("vector-ref", arg => PrimitivesAlg.MakePort(FileMode.Writer, arg)),
      "close-input-port" -> wrapPrimitive("vector-ref", PrimitivesAlg.ClosePort.apply),
      "close-output-port" -> wrapPrimitive("vector-ref", PrimitivesAlg.ClosePort.apply),
      "read" -> wrapPrimitive("vector-ref", PrimitivesAlg.Read.apply),
      "write" -> wrapPrimitive("vector-ref", PrimitivesAlg.Write.apply),
      "read-contents" -> wrapPrimitive("vector-ref", PrimitivesAlg.ReadContents.apply),
    ))