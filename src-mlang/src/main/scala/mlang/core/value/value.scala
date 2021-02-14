package mlang.core.value

import mlang.core._
import utils._

class EvalErrorException() extends Exception()

sealed trait CompoundTerm extends HasTyp

sealed trait Typ extends CompoundTerm
case class Pi(dom: Term, cod: Closure1, et: etyp.Pi) extends Typ:
  override lazy val typ: Sort = Sort.pi(dom.whnf.sort, cod(Generic(dom)).whnf.sort)
case class Record(fields: Telescope, et: etyp.Record) extends Typ:
  override lazy val typ: Sort = Sort.sigma(fields.sorts)
case class Enum(kases: Seq[Telescope], et: etyp.Enum) extends Typ:
  override def toString(): String = s"Enum(${et})"
  def apply(a: Int): Telescope = kases(a)
  override lazy val typ: Sort = Sort.ind(kases.map(b => Sort.sigma(b.sorts)))
case class Eq(t: Term, left: Term, right: Term) extends Typ:
  override def typ: Sort = t.sort

/**
  * non of the intro form is type annotated!
  */
sealed trait Intro extends CompoundTerm
case class Lambda(body: Closure1) extends Intro
case class Make(fields: Seq[Term]) extends Intro
case class Construct(kase: Int, fields: Seq[Term]) extends Intro
case class Branch(pattern: Pattern, clos: Closure)
case class PatternLambda(branches: Seq[Branch]) extends Intro

sealed trait Redux extends CompoundTerm
case class App(head: Term, arg: Term) extends Redux:
  override lazy val typ: Term =
    head.typ.whnf match
    case Pi(a, b, _) => b(arg)
    case _ => logicError()

case class Proj(head: Term, field: Int) extends Redux:
  override lazy val typ: Term =
    head.typ.whnf match
    case Unwrap(Record(fields, _)) => fields.apply(head, field)
    case _ => logicError()

sealed trait Annotated extends HasTyp
sealed trait Inversable extends Annotated
sealed trait Box extends Inversable

class Generic(override val typ: Term) extends Inversable

class Axiom(override val typ: Term) extends Inversable

/**
 * used to prevent reading back the expanded term of e.g. Eq
 */
case class Glue(stuck: Term, reduced: Term) extends Annotated

/**
 * glued a skel and a type-inferable stuck term to a term.
 * they don't have coresponding core term, and they are automately generated when we want to propogate
 * type annotations from a `Def` to terms that evaluated from the `Def`
 * see `Skel.scala`
 */
case class SkelGlue(stuck: Term, reduced: Term, global: Boolean, skel: Skel) extends Annotated:
  override def typ: Term = stuck.typ
object SkelGlue:
  def generic(tp: Term, term: Term, global: Boolean, skel: Skel | Null): Term =
    if skel != null then SkelGlue(Generic(tp), term, global, skel) else term
/**
  * provides indirectness so let-definition and global definitions are readback as references
  */
case class Def(override val typ: Term, var term: Term | UncheckedNull) extends Box

case class Meta(override val typ: Term, var term: Term | Null) extends Box:
  override def toString() = s"Meta(${term != null})@" + System.identityHashCode(this)


type Term = Annotated | Sort | CompoundTerm

object ErrorClosure
type Closure1 = Function1[Term, Term]
type Closure2 = Function2[Term, Term, Term]
type Closure3 = Function3[Term, Term, Term, Term]
type Closure4 = Function4[Term, Term, Term, Term, Term]
type Closure5 = Function5[Term, Term, Term, Term, Term, Term]
type Closure6 = Function6[Term, Term, Term, Term, Term, Term, Term]
type Closure1p = Closure1 | Closure2 | Closure3 | Closure4 | Closure5 | Closure6
type Closure = Term | Closure1p | ErrorClosure.type

extension (c: Closure) def apply(ts: Seq[Term]): Term = 
  c match
  case t: Term if ts.length == 0 => t
  case c: Closure1 if ts.length == 1 => c(ts(0))
  case c: Closure2 if ts.length == 2 => c(ts(0), ts(1))
  case c: Closure3 if ts.length == 3 => c(ts(0), ts(1), ts(2))
  case c: Closure4 if ts.length == 4 => c(ts(0), ts(1), ts(2), ts(3))
  case c: Closure5 if ts.length == 5 => c(ts(0), ts(1), ts(2), ts(3), ts(4))
  case c: Closure6 if ts.length == 6 => c(ts(0), ts(1), ts(2), ts(3), ts(4), ts(5))
  case ErrorClosure if ts.length == 0 => throw EvalErrorException()
  case _ => logicError()

type TelescopeClosure = Function1[Term, Telescope]

enum Telescope:
  case Nil
  case Cons(head: Term, tail: TelescopeClosure)

extension (tele: Telescope)
  def apply(v: Term, i: Int): Term = apply(v, i, 0)
  private def apply(v: Term, i: Int, b: Int): Term = 
    apply((0 until i).map(k => Proj(v, k)), b)
  
  def apply(vs: Seq[Term]): Term =
    apply(vs, 0)

  private def apply(vs: Seq[Term], b: Int): Term =
    tele match
    case Telescope.Cons(h, t) =>
      if vs.size == 0 then
        h
      else
        t(vs.head).apply(vs.tail, b + 1)
    case Telescope.Nil => logicError()

  def sorts: Seq[Sort] =
    tele match
    case Telescope.Cons(h, t) => h.whnf.sort +: t(Generic(h)).sorts
    case Telescope.Nil => Seq.empty

