package mlang.core.syntax.dbi
import mlang.core.syntax._
import mlang.core.{ Sort }
import mlang.infra._
import utils.{ logicError, contract }
import scala.annotation.targetName

case class InsertInfo(l: Int, map: Seq[Int], boxes: Seq[Box.Local])

case class Reorder(from: Int, to: Int)
case class ShiftArgs(
  up: Int,
  inserts: InsertInfo | Null,
  reorder: Reorder | Null,
  depth: Int = 0):
  contract { reorder == null || inserts == null }

case class LiftArgs(l: Int)
case class FindRefArgs(ref: utils.Ref)
private case object RefFoundHack extends Exception

type VisitArgs = ShiftArgs | LiftArgs | FindRefArgs


extension (args: VisitArgs)
  def + (n: Int): VisitArgs =
    args match
    case s: ShiftArgs => s.copy(depth = s.depth + n)
    case _ => args

extension (tele: Telescope)
  private[dbi] def visit(args: VisitArgs): Telescope =
    tele match
    case Telescope.Nil => Telescope.Nil
    case Telescope.Let(defs, in) => Telescope.Let(defs.visit(args), in.visit(args + defs.size))
    case Telescope.Cons(h, t) => Telescope.Cons(h.visit(args), t.visit(args))

  def shift(up: Int, inserts: InsertInfo | Null, reorder: Reorder | Null): Telescope =
    if up == 0 && inserts == null then tele
    else
      val inner = tele.visit(ShiftArgs(up, inserts, reorder))
      if inserts != null then
        Telescope.Let(inserts.boxes, inner)
      else
        inner

extension (defs: Seq[Box.Local])
  private[dbi] def visit(args: VisitArgs): Seq[Box.Local] =
    defs match
    case h +: tail => h.visit(args) +: tail.visit(args + 1)
    case _ => Seq.empty

extension (h: Box.Local)
  private[dbi] def visit(args: VisitArgs): Box.Local =
    h match
    case Box.Def(tp, tm) => Box.Def(tp.visit(args), tm.visit(args))
    case Box.Meta(tp, tm) => Box.Meta(tp.visit(args), tm.visit(args))

  def shift(up: Int): Box.Local = h.visit(ShiftArgs(up, null, null))
  def shiftWithDepth(up: Int, d: Int): Box.Local = h.visit(ShiftArgs(up, null, null, d))


extension (clos: TelescopeClosure)
  @targetName("visit_closure") private[dbi] def visit(args: VisitArgs): TelescopeClosure =
    TelescopeClosure(clos.get.visit(args + 1))

extension (clos: Closure)
  private[dbi] def visit(n: Int, args: VisitArgs): Closure =
    Closure(clos.get.visit(args + n))

extension (term: Term)
  def lift(up: Int): Term = if up == 0 then term else term.visit(LiftArgs(up))
  def findRef(r: utils.Ref): Boolean = 
    try {
       term.visit(FindRefArgs(r))
       false
    } catch {
      case RefFoundHack => true
      case e => throw e
    }
  
  def shift(up: Int, inserts: InsertInfo | Null, reorder: Reorder | Null = null): Term = 
    if up == 0 && inserts == null then term
    else
      val inner = term.visit(ShiftArgs(up, inserts, reorder))
      if inserts != null then
        Let(inserts.boxes, inner)
      else
        inner
    

  private [dbi] def visit(args: VisitArgs): Term =
    term match
    case r: Ref =>
      args match
      case ShiftArgs(up, inserts, reorder, depth) =>
        if r.get > depth then
          val diff: Int = 
            if inserts != null then inserts.boxes.size + up
            else if reorder != null then
              val out = r.get - depth
              val ret = if out > reorder.from && out <= reorder.to then
                -reorder.from
              else if out <= reorder.from then
                reorder.to - reorder.from
              else
                up
              trace.log("" + r + " " + depth + " " + reorder + " " + ret)
              ret
            else up
          Ref(r.get + diff)
        else r
      case _ => r
    case a: TempRef =>
      args match
      case ShiftArgs(up, inserts, reorder, depth) =>
        if inserts != null then
          if a.i1 > inserts.l then logicError()
          else if a.i1 == inserts.l then Ref(depth + inserts.boxes.size - inserts.map(a.i2))
          else a
        else a
      case _ => a
    case Pi(d, c, e) => Pi(d.visit(args), c.visit(1, args), e)
    case Record(fs, e) => Record(fs.visit(args), e)
    case Eq(t, a, b) => Eq(t.visit(args), a.visit(args), b.visit(args))
    case Enum(ks, e) => Enum(ks.map(_.visit(args)), e)
    case Lambda(b) => Lambda(b.visit(1, args))
    case PatternLambda(bs) => PatternLambda(bs.map(b => Branch(b.pattern, b.clos.visit(b.pattern.size, args))))
    case Construct(k, fs) => Construct(k, fs.map(_.visit(args)))
    case Make(fs) => Make(fs.map(_.visit(args)))
    case App(l, r) => App(l.visit(args), r.visit(args))
    case Proj(l, r) => Proj(l.visit(args), r)
    case Let(ds, in) => Let(ds.visit(args), in.visit(args + ds.size))
    case Error => term
    case a: Sort => 
      args match
      case LiftArgs(l) => a.lift(l)
      case _ => a
    case g@GlobalRef(name, a) =>
      args match
      case LiftArgs(l) => GlobalRef(name, a + l)
      case FindRefArgs(r) if r == name => throw RefFoundHack
      case _ => g
 
