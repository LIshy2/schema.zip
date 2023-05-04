package interpreter

import cats.{Applicative, Monad, MonadError, ~>}
import cats.syntax.*
import cats.implicits.*

import java.io.{BufferedReader, File, FileReader, InputStreamReader, PrintWriter}
import java.util.stream.Collectors
import scala.collection.immutable.List

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

def port[M[_]](x: Scheme)(using me: MonadError[M, RuntimeError]): M[PortHandler] =
  x match
    case Scheme.Port(value) => value.pure[M]
    case s@_ => me.raiseError(RuntimeError.TypeMismatch("Port", s))

val stdinPort = Scheme.Port(PortHandler.ReaderPort(BufferedReader(InputStreamReader(System.in))))
val stdoutPort = Scheme.Port(PortHandler.WriterPort(PrintWriter(System.out)))

def fileReaderPort(filename: String): Scheme = Scheme.Port(PortHandler.ReaderPort(BufferedReader(FileReader(filename))))
def fileWriterPort(filename: String): Scheme = Scheme.Port(PortHandler.WriterPort(PrintWriter(new File(filename))))

private enum NumOperatorsAlg[A]:
  case Plus(a: Scheme, b: Scheme) extends NumOperatorsAlg[Scheme]

  case Minus(a: Scheme, b: Scheme) extends NumOperatorsAlg[Scheme]

  case Div(a: Scheme, b: Scheme) extends NumOperatorsAlg[Scheme]

  case Mul(a: Scheme, b: Scheme) extends NumOperatorsAlg[Scheme]

  case Mod(a: Scheme, b: Scheme) extends NumOperatorsAlg[Scheme]


private def numOperatorsCompiler[M[_]](using me: MonadError[M, RuntimeError]): NumOperatorsAlg ~> M =
  new(NumOperatorsAlg ~> M):
    override def apply[A](alg: NumOperatorsAlg[A]): M[A] =
      alg match
        case NumOperatorsAlg.Plus(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Number(aM + bM)

        case NumOperatorsAlg.Minus(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Number(aM - bM)

        case NumOperatorsAlg.Div(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
            _ <- me.raiseWhen(bM == 0 && aM != 0)(RuntimeError.BadSpecialForm("Division by Zero", b))
          yield Scheme.Number(aM / bM)

        case NumOperatorsAlg.Mul(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Number(aM * bM)

        case NumOperatorsAlg.Mod(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
            _ <- me.raiseWhen(bM == 0 && aM != 0)(RuntimeError.BadSpecialForm("Division by Zero", b))
          yield Scheme.Number(aM % bM)

private enum EqNumOperatorsAlg[A]:
  case Eq(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

  case Less(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

  case Great(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

  case NotEq(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

  case LTE(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

  case GTE(a: Scheme, b: Scheme) extends EqNumOperatorsAlg[Scheme]

private def eqNumOperatorCompiler[M[_]](using MonadError[M, RuntimeError]): EqNumOperatorsAlg ~> M =
  new(EqNumOperatorsAlg ~> M):
    override def apply[A](alg: EqNumOperatorsAlg[A]): M[A] =
      alg match
        case EqNumOperatorsAlg.Eq(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM == bM)

        case EqNumOperatorsAlg.Less(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM < bM)

        case EqNumOperatorsAlg.Great(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM > bM)

        case EqNumOperatorsAlg.NotEq(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM != bM)

        case EqNumOperatorsAlg.LTE(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM <= bM)

        case EqNumOperatorsAlg.GTE(a, b) =>
          for
            aM <- num(a)
            bM <- num(b)
          yield Scheme.Bool(aM >= bM)


private enum BoolOperatorsAlg[A]:
  case And(a: Scheme, b: Scheme) extends BoolOperatorsAlg[Scheme]
  case Or(a: Scheme, b: Scheme) extends BoolOperatorsAlg[Scheme]


private def boolOperatorCompiler[M[_]](using MonadError[M, RuntimeError]): BoolOperatorsAlg ~> M =
  new(BoolOperatorsAlg ~> M):
    override def apply[A](alg: BoolOperatorsAlg[A]): M[A] =
      alg match
        case BoolOperatorsAlg.And(a, b) =>
          for
            aBool <- bool(a)
            bBool <- bool(b)
          yield Scheme.Bool(aBool && bBool)

        case BoolOperatorsAlg.Or(a, b) =>
          for
            aBool <- bool(a)
            bBool <- bool(b)
          yield Scheme.Bool(aBool || bBool)


private enum EqStrOperatorsAlg[A]:
  case Eq(a: Scheme, b: Scheme) extends EqStrOperatorsAlg[Scheme]

  case Less(a: Scheme, b: Scheme) extends EqStrOperatorsAlg[Scheme]

  case Great(a: Scheme, b: Scheme) extends EqStrOperatorsAlg[Scheme]

  case LTE(a: Scheme, b: Scheme) extends EqStrOperatorsAlg[Scheme]

  case GTE(a: Scheme, b: Scheme) extends EqStrOperatorsAlg[Scheme]


private def eqStrOperatorCompiler[M[_]](using MonadError[M, RuntimeError]): EqStrOperatorsAlg ~> M =
  new(EqStrOperatorsAlg ~> M):
    override def apply[A](alg: EqStrOperatorsAlg[A]): M[A] =
      alg match
        case EqStrOperatorsAlg.Eq(a, b) =>
          for
            aStr <- str(a)
            bStr <- str(b)
          yield Scheme.Bool(aStr == bStr)

        case EqStrOperatorsAlg.Less(a, b) =>
          for
            aStr <- str(a)
            bStr <- str(b)
          yield Scheme.Bool(aStr < bStr)

        case EqStrOperatorsAlg.Great(a, b) =>
          for
            aStr <- str(a)
            bStr <- str(b)
          yield Scheme.Bool(aStr > bStr)

        case EqStrOperatorsAlg.LTE(a, b) =>
          for
            aStr <- str(a)
            bStr <- str(b)
          yield Scheme.Bool(aStr <= bStr)

        case EqStrOperatorsAlg.GTE(a, b) =>
          for
            aStr <- str(a)
            bStr <- str(b)
          yield Scheme.Bool(aStr >= bStr)


private enum ListOperatorsAlg[A]:
  case Car(arg: Scheme) extends ListOperatorsAlg[Scheme]

  case Cdr(arg: Scheme) extends ListOperatorsAlg[Scheme]

  case Cons(fst: Scheme, snd: Scheme) extends ListOperatorsAlg[Scheme]


private def listOperatorCompiler[M[_]](using me: MonadError[M, RuntimeError]): ListOperatorsAlg ~> M =
  new(ListOperatorsAlg ~> M):
    override def apply[A](alg: ListOperatorsAlg[A]): M[A] =
      alg match
        case ListOperatorsAlg.Car(arg) =>
          arg match
            case Scheme.SList(x :: _) => x.pure[M]
            case Scheme.DottedList(x :: _, _) => x.pure[M]
            case s@_ => me.raiseError(RuntimeError.TypeMismatch("pair", s))

        case ListOperatorsAlg.Cdr(arg) =>
          arg match
            case Scheme.SList(_ :: xs) => Scheme.SList(xs).pure[M]
            case Scheme.DottedList(List(_), snd) => snd.pure[M]
            case Scheme.DottedList(_ :: xs, snd) => Scheme.DottedList(xs, snd).pure[M]
            case s@_ => me.raiseError(RuntimeError.TypeMismatch("pair", s))

        case ListOperatorsAlg.Cons(fst, snd) =>
          (fst, snd) match
            case (x, Scheme.SList(xs)) => Scheme.SList(x :: xs).pure[M]
            case (x, Scheme.DottedList(fst, snd)) => Scheme.DottedList(x :: fst, snd).pure[M]
            case (a, b) => Scheme.DottedList(List(a), b).pure[M]


private enum EqualsOperatorsAlg[A]:
  case Eqv(a: Scheme, b: Scheme) extends EqualsOperatorsAlg[Scheme]

  case Eq(a: Scheme, b: Scheme) extends EqualsOperatorsAlg[Scheme]


private def equalsOperatorCompiler[M[_]](using MonadError[M, RuntimeError]): EqualsOperatorsAlg ~> M =
  new(EqualsOperatorsAlg ~> M):
    override def apply[A](alg: EqualsOperatorsAlg[A]): M[A] =
      alg match
        case EqualsOperatorsAlg.Eqv(a, b) =>
          (a, b) match
            case (Scheme.Bool(valA), Scheme.Bool(valB)) => Scheme.Bool(valA == valB).pure[M]
            case (Scheme.Number(valA), Scheme.Number(valB)) => Scheme.Bool(valA == valB).pure[M]
            case (Scheme.Str(valA), Scheme.Str(valB)) => Scheme.Bool(valA == valB).pure[M]
            case (Scheme.Atom(idA), Scheme.Atom(idB)) => Scheme.Bool(idA == idB).pure[M]
            case (Scheme.SList(Nil), Scheme.SList(Nil)) => Scheme.Bool(true).pure[M]
            case (Scheme.SList(listA), Scheme.SList(listB)) => Scheme.Bool(listA eq listB).pure[M] // reference equality
            case _ => Scheme.Bool(false).pure[M]
        case EqualsOperatorsAlg.Eq(a, b) => this (EqualsOperatorsAlg.Eqv(a, b))

private enum VectorOperatorsAlg[A]:
  case Make(k: Scheme, fill: Scheme) extends VectorOperatorsAlg[Scheme]

  case Ref(x: Scheme, ind: Scheme) extends VectorOperatorsAlg[Scheme]

  case Set(x: Scheme, ind: Scheme, value: Scheme) extends VectorOperatorsAlg[Scheme]


private def vectorOperatorCompiler[M[_]](using me: MonadError[M, RuntimeError]): VectorOperatorsAlg ~> M =
  new(VectorOperatorsAlg ~> M):
    override def apply[A](alg: VectorOperatorsAlg[A]): M[A] =
      alg match
        case VectorOperatorsAlg.Make(k, fill) =>
          for
            kNum <- num(k)
          yield Scheme.Vector(Array.fill(kNum)(fill))

        case VectorOperatorsAlg.Ref(x, ind) =>
          for
            vec <- vector(x)
            i <- num(ind)
            _ <- me.raiseUnless(0 <= i && i < vec.length)(RuntimeError.BadSpecialForm("Vector out of bounds", ind))
          yield vec(i)

        case VectorOperatorsAlg.Set(x, ind, value) =>
          for
            vec <- vector(x)
            i <- num(ind)
            _ <- me.raiseUnless(0 <= i && i < vec.length)(RuntimeError.BadSpecialForm("Vector out of bounds", ind))
            _ = vec.update(i, value)
          yield Scheme.Vector(vec)


enum FileMode:
  case Reader
  case Writer

enum IOOperatorsAlg[A]:
  case MakePort(mode: FileMode, filename: Scheme) extends IOOperatorsAlg[Scheme]

  case ClosePort(port: Scheme) extends IOOperatorsAlg[Scheme]

  case Read(port: Scheme) extends IOOperatorsAlg[Scheme]

  case Write(value: Scheme, port: Scheme) extends IOOperatorsAlg[Scheme]

  case ReadContents(filename: Scheme) extends IOOperatorsAlg[Scheme]


private def ioOperatorCompiler[M[_]](using me: MonadError[M, RuntimeError]): IOOperatorsAlg ~> M =
  new(IOOperatorsAlg ~> M):
    override def apply[A](alg: IOOperatorsAlg[A]): M[A] =
      alg match
        case IOOperatorsAlg.MakePort(mode, filename) =>
          for
            filenameM <- str(filename)
            result <- mode match
              case FileMode.Reader => fileReaderPort(filenameM).pure[M]
              case FileMode.Writer => fileWriterPort(filenameM).pure[M]
          yield result

        case IOOperatorsAlg.ClosePort(p) =>
          for
            portM <- port(p)
            _ = portM match
              case PortHandler.ReaderPort(br) => br.close()
              case PortHandler.WriterPort(pw) => pw.close()
          yield Scheme.Port(portM)

        case IOOperatorsAlg.Read(p) =>
          for
            portM <- port(p)
            result <- portM match
              case PortHandler.ReaderPort(br) => br.readLine().pure[M]
              case _ => me.raiseError(RuntimeError.BadSpecialForm("Wrong port mode", p))
          yield Scheme.Str(result)


        case IOOperatorsAlg.Write(value, p) =>
          for
            portM <- port(p)
            _ = portM match
              case PortHandler.WriterPort(pw) => pw.write(mkString(value))
              case _ => me.raiseError(RuntimeError.BadSpecialForm("Wrong port mode", p))
          yield value


        case IOOperatorsAlg.ReadContents(filename) =>
          for
            filenameM <- str(filename)
            port = fileReaderPort(filenameM)
            result <- port match
              case Scheme.Port(PortHandler.ReaderPort(br)) => br.lines().collect(Collectors.joining(System.lineSeparator())).pure[M]
              case _ => me.raiseError(RuntimeError.BadSpecialForm("Wrong port mode", port))
          yield Scheme.Str(result)


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

  def makePort(mode: FileMode, arg: List[Scheme]): M[Scheme]

  def closePort(arg: List[Scheme]): M[Scheme]

  def read(arg: List[Scheme]): M[Scheme]

  def write(arg: List[Scheme]): M[Scheme]

  def readContents(arg: List[Scheme]): M[Scheme]


// All algebras in one + arg count checking
enum PrimitivesAlg[A]:

  case Plus(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Minus(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Div(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Mul(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Mod(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumEq(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumLess(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumGreat(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumNotEq(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumLTE(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case NumGTE(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case And(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Or(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case StrEq(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case StrLess(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case StrGreat(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case StrLTE(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case StrGTE(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Car(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Cdr(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case Cons(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case DataEqv(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case DataEq(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case VectorMake(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case VectorRef(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case VectorSet(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

  case MakePort(mode: FileMode, arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case ClosePort(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case Read(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case Write(arg: List[Scheme]) extends PrimitivesAlg[Scheme]


  case ReadContents(arg: List[Scheme]) extends PrimitivesAlg[Scheme]

private def primitivesCompiler[M[_]](using me: MonadError[M, RuntimeError]): PrimitivesAlg ~> M =
  new(PrimitivesAlg ~> M):
    override def apply[A](alg: PrimitivesAlg[A]): M[A] =
      alg match
        case PrimitivesAlg.Plus(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => numOperatorsCompiler(NumOperatorsAlg.Plus(a, b))))

        case PrimitivesAlg.Minus(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => numOperatorsCompiler(NumOperatorsAlg.Minus(a, b))))

        case PrimitivesAlg.Div(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => numOperatorsCompiler(NumOperatorsAlg.Div(a, b))))

        case PrimitivesAlg.Mul(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => numOperatorsCompiler(NumOperatorsAlg.Mul(a, b))))

        case PrimitivesAlg.Mod(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => numOperatorsCompiler(NumOperatorsAlg.Mod(a, b))))

        case PrimitivesAlg.NumEq(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.Eq(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.NumLess(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.Less(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.NumGreat(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.Great(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.NumNotEq(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.NotEq(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.NumLTE(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.LTE(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.NumGTE(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqNumOperatorCompiler(EqNumOperatorsAlg.GTE(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.And(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => boolOperatorCompiler(BoolOperatorsAlg.And(a, b))))


        case PrimitivesAlg.Or(arg: List[Scheme]) =>
          arg match
            case s@List(_) => me.raiseError(RuntimeError.ArgsCount(2, s))
            case list => list.map(_.pure[M]).reduce(operatorM((a, b) => boolOperatorCompiler(BoolOperatorsAlg.Or(a, b))))

        case PrimitivesAlg.StrEq(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqStrOperatorCompiler(EqStrOperatorsAlg.Eq(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.StrLess(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqStrOperatorCompiler(EqStrOperatorsAlg.Less(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.StrGreat(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqStrOperatorCompiler(EqStrOperatorsAlg.Great(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.StrLTE(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqStrOperatorCompiler(EqStrOperatorsAlg.LTE(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.StrGTE(arg: List[Scheme]) =>
          arg match
            case List(a, b) => eqStrOperatorCompiler(EqStrOperatorsAlg.GTE(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.Car(arg: List[Scheme]) =>
          arg match
            case List(list) => listOperatorCompiler(ListOperatorsAlg.Car(list))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))

        case PrimitivesAlg.Cdr(arg: List[Scheme]) =>
          arg match
            case List(list) => listOperatorCompiler(ListOperatorsAlg.Cdr(list))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))

        case PrimitivesAlg.Cons(arg: List[Scheme]) =>
          arg match
            case List(a, b) => listOperatorCompiler(ListOperatorsAlg.Cons(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


        case PrimitivesAlg.DataEqv(arg: List[Scheme]) =>
          arg match
            case List(a, b) => equalsOperatorCompiler(EqualsOperatorsAlg.Eqv(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


        case PrimitivesAlg.DataEq(arg: List[Scheme]) =>
          arg match
            case List(a, b) => equalsOperatorCompiler(EqualsOperatorsAlg.Eq(a, b))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))

        case PrimitivesAlg.VectorMake(arg: List[Scheme]) =>
          arg match
            case List(k, fill) => vectorOperatorCompiler(VectorOperatorsAlg.Make(k, fill))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


        case PrimitivesAlg.VectorRef(arg: List[Scheme]) =>
          arg match
            case List(x, ind) => vectorOperatorCompiler(VectorOperatorsAlg.Ref(x, ind))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


        case PrimitivesAlg.VectorSet(arg: List[Scheme]) =>
          arg match
            case List(x, ind, value) => vectorOperatorCompiler(VectorOperatorsAlg.Set(x, ind, value))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(3, s))

        case PrimitivesAlg.MakePort(mode: FileMode, arg: List[Scheme]) =>
          arg match
            case List(filename) => ioOperatorCompiler(IOOperatorsAlg.MakePort(mode, filename))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))


        case PrimitivesAlg.ClosePort(arg: List[Scheme]) =>
          arg match
            case List(port) => ioOperatorCompiler(IOOperatorsAlg.ClosePort(port))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))


        case PrimitivesAlg.Read(arg: List[Scheme]) =>
          arg match
            case List() => ioOperatorCompiler(IOOperatorsAlg.Read(stdinPort))
            case List(port) => ioOperatorCompiler(IOOperatorsAlg.Read(port))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))


        case PrimitivesAlg.Write(arg: List[Scheme]) =>
          arg match
            case List(value) => ioOperatorCompiler(IOOperatorsAlg.Write(value, stdoutPort))
            case List(value, port) => ioOperatorCompiler(IOOperatorsAlg.Write(value, port))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(2, s))


        case PrimitivesAlg.ReadContents(arg: List[Scheme]) =>
          arg match
            case List(filename) => ioOperatorCompiler(IOOperatorsAlg.ReadContents(filename))
            case s@_ => me.raiseError(RuntimeError.ArgsCount(1, s))


def operatorM[M[_] : Monad](operator: (Scheme, Scheme) => M[Scheme]): (M[Scheme], M[Scheme]) => M[Scheme] =
  (aM: M[Scheme], bM: M[Scheme]) =>
    for
      a <- aM
      b <- bM
      result <- operator(a, b)
    yield result

