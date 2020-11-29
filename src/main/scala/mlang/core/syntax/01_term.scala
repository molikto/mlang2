package mlang.core.syntax
import mlang.core.common
import common.{ Sort, Pattern, Name, EType }

opaque type Ref = Int
object Ref:
  def apply(i: Int): Ref = i
  extension (r: Ref) def get: Int = r

case class Pi(a: Term, b: Closure, etype: EType.Pi)
case class Lambda(m: Closure)
case class App(m: Term, n: Term)

case class Record(fs: Telescope, etype: EType.Record)
case class Make(ts: Seq[Term])
case class Proj(m: Term, f: Int)

case class Sum(bs: Seq[Telescope], etype: EType.Sum)
case class Construct(b: Int, ts: Seq[Term])
// used for both record & construct, record is both negative & positive
case class Case(pattern: Pattern, closure: Closure)
case class PatternLambda(cases: Seq[Case])

case class Let(defs: Seq[Term], in: Term)

type Term = Ref | Sort | Pi | Lambda | App | Record | Make | Proj | Sum | Construct | PatternLambda | Let


opaque type Closure = Term
object Closure:
  def apply(i: Term): Closure = i
  extension (c: Closure) def get: Term = c

opaque type Telescope = Seq[Term | Closure]
object Telescope:
  def apply(ts: (Term | Closure)*): Telescope = ts
