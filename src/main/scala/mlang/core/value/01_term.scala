package mlang.core.value

import mlang.core.common
import mlang.core.syntax
import scala.collection.mutable
import common.{ Pattern, Sort, EType }

import mlang.utils._

type Env = Seq[Term]
type TEnv = Seq[Term]

class Var()

case class Pi(a: Term, b: Closure1, etype: EType.Pi)
case class Lambda(m: Closure1)
case class App(m: Term, n: Term)

case class Record(fs: Telescope, etype: EType.Record)
case class Make(ts: Seq[Term])
case class Proj(m: Term, f: Int)

case class Sum(bs: Seq[Telescope], etype: EType.Sum)
case class Construct(b: Int, ts: Seq[Term])

case class PatternLambda(cases: Seq[Case])

type Term = Var | Sort | Pi | Lambda | App | Record | Make | Proj | Sum | Construct | PatternLambda

def constructWhnf(lambda: PatternLambda, stuck: Term): Term = {
  var res: Term = null
  var cs = lambda.cases
  while (cs.nonEmpty && res == null) {
    res = cs.head.tryApp(stuck).orNull
    cs = cs.tail
  }
  if (res == null) {
    val ss: Term = stuck.whnf
    if (ss == stuck) this else App(lambda, ss)
  } else {
    res.whnf
  }
  return res
}

extension (term: Term):
  def whnf: Term = 
    term match
    case App(l, r) =>
      l.whnf match
      case Lambda(m) => m(r).whnf
      case l@PatternLambda(cases) => constructWhnf(l, r)
      case _ => term
    case Proj(l, r) =>
      l.whnf match
      case Make(ts) => ts(r).whnf
      case _ => term
    case _ => term

extension (syn: syntax.Term):
  def eval(using env: Env): Term =
    syn match
    case a: Sort => a
    case ref: syntax.Ref => env(ref.get)
    case syntax.Pi(a, b, e) => Pi(a.eval, b.eval1, e)
    case syntax.Lambda(a) => Lambda(a.eval1)
    case syntax.App(a, b) => App(a.eval, b.eval)
    case syntax.Record(fs, e) => Record(fs.eval, e)
    case syntax.Make(ts) => Make(ts.map(_.eval))
    case syntax.Proj(v, i) => Proj(v.eval, i)
    case syntax.Sum(bs, e) => Sum(bs.map(_.eval), e)
    case syntax.Construct(b, ts) => Construct(b, ts.map(_.eval))
    case syntax.PatternLambda(cases) => PatternLambda(cases.map(c => Case(c.pattern, c.closure.eval(c.pattern.size))))


opaque type Telescope = Seq[Closure]
object Telescope:
  def apply(ts: (Closure)*): Telescope = ts

extension (t: syntax.Telescope) def eval(using env: Env): Telescope = ???


type Closure1 = Function1[Term, Term]
type Closure2 = Function2[Term, Term, Term]
type Closure3 = Function3[Term, Term, Term, Term]
type Closure4 = Function4[Term, Term, Term, Term, Term]
type Closure5 = Function5[Term, Term, Term, Term, Term, Term]
type Closure6 = Function6[Term, Term, Term, Term, Term, Term, Term]
type Closure = Term | Closure1 | Closure2 | Closure3 | Closure4 | Closure5 | Closure6

extension (c: Closure) def apply(ts: Seq[Term]): Term = 
  c match
  case t: Term if ts.length == 0 => t
  case c: Closure1 if ts.length == 1 => c(ts(0))
  case c: Closure2 if ts.length == 2 => c(ts(0), ts(1))
  case c: Closure3 if ts.length == 3 => c(ts(0), ts(1), ts(2))
  case c: Closure4 if ts.length == 4 => c(ts(0), ts(1), ts(2), ts(3))
  case c: Closure5 if ts.length == 5 => c(ts(0), ts(1), ts(2), ts(3), ts(4))
  case c: Closure6 if ts.length == 6 => c(ts(0), ts(1), ts(2), ts(3), ts(4), ts(5))
  case _ => logicError()


class RawClosure1(env: Env, b: syntax.Closure) extends Closure1:
  def apply(v: Term): Term = eval(b.get)(using env :+ v)

extension (b: syntax.Closure) def eval1(using env: Env): Closure1 = RawClosure1(env, b)

extension (b: syntax.Closure) def eval(n: Int)(using env: Env): Closure = ???


case class Case(pattern: Pattern, closure: Closure):
  private def extract(pattern: Pattern, v: Term): Option[Seq[Term]] = {
  val vs = mutable.ArrayBuffer[Term]()
  def recAll(ps: Seq[Pattern], vs: Seq[Term]): Boolean = {
    if (ps.size != vs.size) logicError()
    var i = 0
    var ret = true
    while (i < ps.length) {
      if (!rec(ps(i), vs(i))) {
        ret = false
        i = ps.length
      }
      i += 1
    }
    return ret
  }
  def rec(pattern: Pattern, v: Term): Boolean = {
    pattern match {
    case Pattern.Generic =>
      vs.append(v); true
    case Pattern.Make(names) =>
      v.whnf match {
      case Make(values) => recAll(names, values)
      case _ => false
      }
    case Pattern.Construct(name, pt) =>
      v.whnf match {
      case Construct(n, values) if name == n => recAll(pt, values)
      case _ => false
      }
    }
  }
  if (rec(pattern, v)) {
    Some(vs.toSeq)
  } else {
    None
  }
  }

  def tryApp(v: Term): Option[Term] = extract(pattern, v).map(v => closure(v))