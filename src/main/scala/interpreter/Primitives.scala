package interpreter

import cats.{Applicative, Monad, MonadError}
import cats.syntax.*
import cats.implicits.*

def num[M[_]](x: Scheme)(using me: MonadError[M, RuntimeError]): M[Int] =
  x match
    case Scheme.Number(value) => value.pure[M]
    case s@_ => me.raiseError(RuntimeError.TypeMismatch("Number", s))

def bool[M[_]](x: Scheme)(using me: MonadError[M, RuntimeError]): M[Boolean] =
  x match
    case Scheme.Bool(value) => value.pure[M]
    case s@_ => me.raiseError(RuntimeError.TypeMismatch("Boolean", s))

def str[M[_]](x: Scheme)(using me: MonadError[M, RuntimeError]): M[String] =
  x match
    case Scheme.Str(value) => value.pure[M]
    case s@_ => me.raiseError(RuntimeError.TypeMismatch("Str", s))

def vector[M[_]](x: Scheme)(using me: MonadError[M, RuntimeError]): M[Array[Scheme]] =
  x match
    case Scheme.Vector(value) => value.pure[M]
    case s@_ => me.raiseError(RuntimeError.TypeMismatch("Vector", s))


trait NumOperators[M[_]]:
  def plus(a: Scheme, b: Scheme): M[Scheme]

  def minus(a: Scheme, b: Scheme): M[Scheme]

  def div(a: Scheme, b: Scheme): M[Scheme]

  def mul(a: Scheme, b: Scheme): M[Scheme]

  def mod(a: Scheme, b: Scheme): M[Scheme]

object NumOperators:
  def apply[M[_]](using op: NumOperators[M]): NumOperators[M] = op

given numOperator[M[_]] (using me: MonadError[M, RuntimeError]): NumOperators[M] with
  def plus(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Number(aM + bM)

  def minus(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Number(aM - bM)

  def div(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
      _ <- me.raiseWhen(bM == 0 && aM != 0)(RuntimeError.BadSpecialForm("Division by Zero", b))
    yield Scheme.Number(aM / bM)

  def mul(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Number(aM * bM)

  def mod(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Number(aM % bM)

trait EqNumOperators[M[_]]:
  def eq(a: Scheme, b: Scheme): M[Scheme]

  def less(a: Scheme, b: Scheme): M[Scheme]

  def great(a: Scheme, b: Scheme): M[Scheme]

  def notEq(a: Scheme, b: Scheme): M[Scheme]

  def LTE(a: Scheme, b: Scheme): M[Scheme]

  def GTE(a: Scheme, b: Scheme): M[Scheme]

object EqNumOperators:
  def apply[M[_]](using op: EqNumOperators[M]): EqNumOperators[M] = op

given eqNumOperator[M[_]] (using me: MonadError[M, RuntimeError]): EqNumOperators[M] with
  def eq(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM == bM)

  def less(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM < bM)

  def great(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM > bM)

  def notEq(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM != bM)

  def LTE(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM <= bM)

  def GTE(a: Scheme, b: Scheme): M[Scheme] =
    for
      aM <- num(a)
      bM <- num(b)
    yield Scheme.Bool(aM >= bM)


trait BoolOperators[M[_]]:
  def and(a: Scheme, b: Scheme): M[Scheme]

  def or(a: Scheme, b: Scheme): M[Scheme]


object BoolOperators:
  def apply[M[_]](using op: BoolOperators[M]): BoolOperators[M] = op


given boolOperator[M[_]] (using me: MonadError[M, RuntimeError]): BoolOperators[M] with
  def and(a: Scheme, b: Scheme): M[Scheme] =
    for
      aBool <- bool(a)
      bBool <- bool(b)
    yield Scheme.Bool(aBool && bBool)

  def or(a: Scheme, b: Scheme): M[Scheme] =
    for
      aBool <- bool(a)
      bBool <- bool(b)
    yield Scheme.Bool(aBool || bBool)


trait EqStrOperators[M[_]]:
  def eq(a: Scheme, b: Scheme): M[Scheme]

  def less(a: Scheme, b: Scheme): M[Scheme]

  def great(a: Scheme, b: Scheme): M[Scheme]

  def LTE(a: Scheme, b: Scheme): M[Scheme]

  def GTE(a: Scheme, b: Scheme): M[Scheme]

object EqStrOperators:
  def apply[M[_]](using op: EqStrOperators[M]): EqStrOperators[M] = op


given eqStrOperators[M[_]] (using me: MonadError[M, RuntimeError]): EqStrOperators[M] with
  def eq(a: Scheme, b: Scheme): M[Scheme] =
    for
      aStr <- str(a)
      bStr <- str(b)
    yield Scheme.Bool(aStr == bStr)

  def less(a: Scheme, b: Scheme): M[Scheme] =
    for
      aStr <- str(a)
      bStr <- str(b)
    yield Scheme.Bool(aStr < bStr)

  def great(a: Scheme, b: Scheme): M[Scheme] =
    for
      aStr <- str(a)
      bStr <- str(b)
    yield Scheme.Bool(aStr > bStr)

  def LTE(a: Scheme, b: Scheme): M[Scheme] =
    for
      aStr <- str(a)
      bStr <- str(b)
    yield Scheme.Bool(aStr <= bStr)

  def GTE(a: Scheme, b: Scheme): M[Scheme] =
    for
      aStr <- str(a)
      bStr <- str(b)
    yield Scheme.Bool(aStr >= bStr)


trait ListOperators[M[_]]:
  def car(arg: Scheme): M[Scheme]

  def cdr(arg: Scheme): M[Scheme]

  def cons(fst: Scheme, snd: Scheme): M[Scheme]

object ListOperators:
  def apply[M[_]](using op: ListOperators[M]): ListOperators[M] = op


given listOperators[M[_]] (using me: MonadError[M, RuntimeError]): ListOperators[M] with

  def car(arg: Scheme): M[Scheme] =
    arg match
      case Scheme.SList(x :: _) => x.pure[M]
      case Scheme.DottedList(x :: _, _) => x.pure[M]
      case s@_ => me.raiseError(RuntimeError.TypeMismatch("pair", s))

  def cdr(arg: Scheme): M[Scheme] =
    arg match
      case Scheme.SList(_ :: xs) => Scheme.SList(xs).pure[M]
      case Scheme.DottedList(List(_), snd) => snd.pure[M]
      case Scheme.DottedList(_ :: xs, snd) => Scheme.DottedList(xs, snd).pure[M]
      case s@_ => me.raiseError(RuntimeError.TypeMismatch("pair", s))

  def cons(fst: Scheme, snd: Scheme): M[Scheme] =
    (fst, snd) match
      case (x, Scheme.SList(xs)) => Scheme.SList(x :: xs).pure[M]
      case (x, Scheme.DottedList(fst, snd)) => Scheme.DottedList(x :: fst, snd).pure[M]
      case (a, b) => Scheme.DottedList(List(a), b).pure[M]


trait EqualsOperators[M[_]]:
  def eqv(a: Scheme, b: Scheme): M[Scheme]

  def eq(a: Scheme, b: Scheme): M[Scheme]


object EqualsOperators:
  def apply[M[_]](using op: EqualsOperators[M]): EqualsOperators[M] = op


given equalsOperators[M[_] : Monad]: EqualsOperators[M] with
  def eqv(a: Scheme, b: Scheme): M[Scheme] =
    (a, b) match
      case (Scheme.Bool(valA), Scheme.Bool(valB)) => Scheme.Bool(valA == valB).pure[M]
      case (Scheme.Number(valA), Scheme.Number(valB)) => Scheme.Bool(valA == valB).pure[M]
      case (Scheme.Str(valA), Scheme.Str(valB)) => Scheme.Bool(valA == valB).pure[M]
      case (Scheme.Atom(idA), Scheme.Atom(idB)) => Scheme.Bool(idA == idB).pure[M]
      case (Scheme.SList(listA), Scheme.SList(listB)) => Scheme.Bool(listA eq listB).pure[M]
      case (Scheme.Vector(arrayA), Scheme.Vector(arrayB)) => Scheme.Bool(arrayA eq arrayB).pure[M]
      case (Scheme.Character(valA), Scheme.Character(valB)) => Scheme.Bool(valA == valB).pure[M]
      case _ => Scheme.Bool(false).pure[M]

  def eq(a: Scheme, b: Scheme): M[Scheme] = eqv(a, b)

trait VectorOperators[M[_]]:
  def make(k: Scheme, fill: Scheme): M[Scheme]

  def ref(x: Scheme, ind: Scheme): M[Scheme]

  def set(x: Scheme, ind: Scheme, value: Scheme): M[Scheme]

object VectorOperators:
  def apply[M[_]](using op: VectorOperators[M]): VectorOperators[M] = op


given vectorOperators[M[_]] (using me: MonadError[M, RuntimeError]): VectorOperators[M] with
  def make(k: Scheme, fill: Scheme): M[Scheme] =
    for
      kNum <- num(k)
    yield Scheme.Vector(Array.fill(kNum)(fill))


  def ref(x: Scheme, ind: Scheme): M[Scheme] =
    for
      vec <- vector(x)
      i <- num(ind)
      _ <- me.raiseUnless(0 <= i && i < vec.length)(RuntimeError.BadSpecialForm("Vector out of bounds", ind))
    yield vec(i)

  def set(x: Scheme, ind: Scheme, value: Scheme): M[Scheme] =
    for
      vec <- vector(x)
      i <- num(ind)
      _ <- me.raiseUnless(0 <= i && i < vec.length)(RuntimeError.BadSpecialForm("Vector out of bounds", ind))
      _ = vec.update(i, value)
    yield Scheme.Vector(vec)


// All algebras in one + arg count checking
trait Primitives[M[_]]:

  def plus(arg: List[Scheme]): M[Scheme]

  def minus(arg: List[Scheme]): M[Scheme]

  def div(arg: List[Scheme]): M[Scheme]

  def mul(arg: List[Scheme]): M[Scheme]

  def mod(arg: List[Scheme]): M[Scheme]

  def numEq(arg: List[Scheme]): M[Scheme]

  def numLess(arg: List[Scheme]): M[Scheme]

  def numGreat(arg: List[Scheme]): M[Scheme]

  def numNotEq(arg: List[Scheme]): M[Scheme]

  def numLTE(arg: List[Scheme]): M[Scheme]

  def numGTE(arg: List[Scheme]): M[Scheme]

  def and(arg: List[Scheme]): M[Scheme]

  def or(arg: List[Scheme]): M[Scheme]

  def strEq(arg: List[Scheme]): M[Scheme]

  def strLess(arg: List[Scheme]): M[Scheme]

  def strGreat(arg: List[Scheme]): M[Scheme]

  def strLTE(arg: List[Scheme]): M[Scheme]

  def strGTE(arg: List[Scheme]): M[Scheme]

  def car(arg: List[Scheme]): M[Scheme]

  def cdr(arg: List[Scheme]): M[Scheme]

  def cons(arg: List[Scheme]): M[Scheme]

  def dataEqv(arg: List[Scheme]): M[Scheme]

  def dataEq(arg: List[Scheme]): M[Scheme]

  def vectorMake(arg: List[Scheme]): M[Scheme]

  def vectorRef(arg: List[Scheme]): M[Scheme]

  def vectorSet(arg: List[Scheme]): M[Scheme]


object Primitives:
  def apply[M[_]](using op: Primitives[M]): Primitives[M] = op


given primitives[M[_] : NumOperators : EqNumOperators : BoolOperators : EqStrOperators : ListOperators : EqualsOperators] (using me: MonadError[M, RuntimeError]): Primitives[M] with
  def plus(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(NumOperators[M].plus))

  def minus(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(NumOperators[M].minus))

  def div(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(NumOperators[M].div))

  def mul(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(NumOperators[M].mul))

  def mod(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(NumOperators[M].mod))

  def numEq(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].eq(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def numLess(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].less(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def numGreat(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].great(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def numNotEq(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].notEq(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def numLTE(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].LTE(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def numGTE(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqNumOperators[M].GTE(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def and(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(BoolOperators[M].and))


  def or(arg: List[Scheme]): M[Scheme] =
    arg match
      case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
      case list => list.map(_.pure[M]).reduce(operatorM(BoolOperators[M].or))

  def strEq(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqStrOperators[M].eq(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def strLess(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqStrOperators[M].less(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def strGreat(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqStrOperators[M].great(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def strLTE(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqStrOperators[M].LTE(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def strGTE(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqStrOperators[M].GTE(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def car(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(list) => ListOperators[M].car(list)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))

  def cdr(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(list) => ListOperators[M].cdr(list)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))

  def cons(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => ListOperators[M].cons(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


  def dataEqv(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqualsOperators[M].eqv(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


  def dataEq(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(a, b) => EqualsOperators[M].eq(a, b)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

  def vectorMake(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(k, fill) => VectorOperators[M].make(k, fill)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


  def vectorRef(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(x, ind) => VectorOperators[M].ref(x, ind)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


  def vectorSet(arg: List[Scheme]): M[Scheme] =
    arg match
      case List(x, ind, value) => VectorOperators[M].set(x, ind, value)
      case s@_ => me.raiseError(RuntimeError.ArgsCount(3, s))


def operatorM[M[_] : Monad](operator: (Scheme, Scheme) => M[Scheme]): (M[Scheme], M[Scheme]) => M[Scheme] =
  (aM: M[Scheme], bM: M[Scheme]) =>
    for
      a <- aM
      b <- bM
      result <- operator(a, b)
    yield result