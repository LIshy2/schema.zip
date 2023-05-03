package interpreter

import cats.syntax.*
import cats.implicits.*
import cats.MonadError

class Context(val state: Map[String, Scheme]):

  def exists[M[_]](id: String): Boolean = state.contains(id)

  def get[M[_]](id: String)(using me: MonadError[M, RuntimeError]): M[Scheme] = me.fromOption(state.get(id), RuntimeError.UnboundVar("", id))

  def set[M[_]](id: String, value: Scheme)(using me: MonadError[M, RuntimeError]): M[Context] =
    if this.exists(id) then
      Context(state.updated(id, value)).pure[M]
    else me.raiseError(RuntimeError.UnboundVar("", id))

  def define(id: String, value: Scheme): Context = Context(state.updated(id, value))

  def provide(newContext: List[(String, Scheme)]): Context =
    newContext.foldLeft(this) {
      case (acc, (id, value)) => acc.define(id, value)
    }

object Context:
  def empty[M[_]]: Context = new Context(Map.empty[String, Scheme])

  def withPrimitives[M[_] : Primitives]: Context =
    def wrapPrimitive(name: String, f: List[Scheme] => M[Scheme]): Scheme =
      Scheme.PrimitiveOperator(PrimitiveFunc(name, f))

    new Context(Map[String, Scheme](
      "+" -> wrapPrimitive("+", Primitives[M].plus),
      "-" -> wrapPrimitive("+", Primitives[M].minus),
      "/" -> wrapPrimitive("/", Primitives[M].div),
      "*" -> wrapPrimitive("*", Primitives[M].mul),
      "mod" -> wrapPrimitive("mod", Primitives[M].mod),
      "=" -> wrapPrimitive("=", Primitives[M].numEq),
      "<" -> wrapPrimitive("<", Primitives[M].numLess),
      ">" -> wrapPrimitive(">", Primitives[M].numGreat),
      "/=" -> wrapPrimitive("/=", Primitives[M].numNotEq),
      "<=" -> wrapPrimitive("<=", Primitives[M].numLTE),
      ">=" -> wrapPrimitive(">=", Primitives[M].numGTE),
      "&&" -> wrapPrimitive("&&", Primitives[M].and),
      "||" -> wrapPrimitive("||", Primitives[M].or),
      "string=?" -> wrapPrimitive("string=?", Primitives[M].strEq),
      "string<?" -> wrapPrimitive("string<?", Primitives[M].strLess),
      "string>?" -> wrapPrimitive("string>?", Primitives[M].strGreat),
      "string<=?" -> wrapPrimitive("string<=?", Primitives[M].strLTE),
      "string>=?" -> wrapPrimitive("string>=?", Primitives[M].strGTE),
      "car" -> wrapPrimitive("car", Primitives[M].car),
      "cdr" -> wrapPrimitive("cdr", Primitives[M].cdr),
      "cons" -> wrapPrimitive("cons", Primitives[M].cons),
      "eq?" -> wrapPrimitive("eq?", Primitives[M].dataEq),
      "eqv?" -> wrapPrimitive("eqv?", Primitives[M].dataEqv),
      "make-vector" -> wrapPrimitive("make-vector", Primitives[M].vectorMake),
      "vector-set!" -> wrapPrimitive("vector-set!", Primitives[M].vectorSet),
      "vector-ref" -> wrapPrimitive("vector-ref", Primitives[M].vectorRef),
    ))