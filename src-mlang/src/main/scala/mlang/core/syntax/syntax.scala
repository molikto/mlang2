package mlang.core.syntax

import mlang.core._
import utils._
import scala.annotation.targetName


/**
  * top level program is the only possible place a recursive definition can be defined
  */
type Program = Seq[Box]

sealed trait CompoundTerm

case class Pi(dom: Term, cod: Closure, et: etyp.Pi = etyp.Pi(Plicit("", Plicity.Ex))) extends CompoundTerm
case class Record(fields: Telescope, et: etyp.Record) extends CompoundTerm
case class Enum(kases: Seq[Telescope], et: etyp.Enum) extends CompoundTerm
case class Eq(t: Term, left: Term, right: Term) extends CompoundTerm

case class Lambda private (body: Closure) extends CompoundTerm

object Lambda:
  @targetName("apply_reordered") def apply(body: Closure): Term =
    val t = new Lambda(body)
    dbi.liftImmAppUp(t, t, 0)

case class Construct(kase: Int, fields: Seq[Term]) extends CompoundTerm
object Construct:
   @targetName("apply_varargs") def apply(kase: Int, fields: Term*) = new Construct(kase, fields)
case class Make(fields: Seq[Term]) extends CompoundTerm
case class Branch(pattern: Pattern, clos: Closure)
case class PatternLambda(branches: Seq[Branch]) extends CompoundTerm

case class App(head: Term, arg: Term) extends CompoundTerm
case class Proj(head: Term, field: Int) extends CompoundTerm

case class TempRef(i1: Int, i2: Int) // not part of core syntax if we don't elaborate!

case class Ref(get: Int):
  def + (i: Int): Ref = Ref(get + i)
  def - (i: Int): Ref = Ref(get - i)
// opaque type Ref = Int
// object Ref:
//   def apply(get: Int): Ref = get
//   extension (r: Ref)
//     def get: Int = r
//     def + (i: Int): Ref = r + i
//     def - (i: Int): Ref = r - i

case class GlobalRef(name: utils.Ref, l: Int)

type RefLike = Ref | TempRef | GlobalRef

case class Closure(get: Term)
// opaque type Closure = Term
// object Closure:
//   def apply(term: Term): Closure = term
//   extension (clo: Closure) def get: Term = clo

case object Error extends CompoundTerm

//  so we don't consider use iterated defs yet. Let(a: Box, in: Term) (?)
case class Let private (defs: Seq[Box.Local], in: Term) extends CompoundTerm // each def intros a binder
object Let:
  @targetName("apply_unwrap") def apply(defs: Seq[Box.Local], in: Term): Term = if defs.isEmpty then in else new Let(defs, in)

type Term = RefLike | Sort | CompoundTerm

sealed trait Box
object Box:
  // **can be both local and global**. no binder intro for term, so non-recursive by definition
  sealed trait Local extends Box
  sealed trait CanRef extends Box
  case class Def(typ: Term, term: Term) extends Local with CanRef
  case class Meta(typ: Term, term: Term) extends Local
  // global only ones, recursive definitions are always decl first, then give the term second by DefRec
  case class Decl(typ: Term) extends Box with CanRef
  case class DefRec(ref: Ref, term: Term) extends Box

case class TelescopeClosure(get: Telescope)
// opaque type TelescopeClosure = Telescope
// object TelescopeClosure:
//   def apply(telescope: Telescope): TelescopeClosure = telescope
//   extension (telescope: TelescopeClosure) def get: Telescope = telescope

/**
  * telescope syntax allowing let definitions, most of time to host boxes
  * but it adds feature for free: definitions inside telescope
  */
sealed trait Telescope
object Telescope:
  case object Nil extends Telescope
  case class Let (defs: Seq[Box.Local], in: Telescope) extends Telescope // each def intros a binder
  case class Cons(head: Term, tail: TelescopeClosure) extends Telescope
