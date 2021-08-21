package mlang.core.value

import mlang.core._
import utils._
import scala.collection.mutable

object Unwrap:
  def unapply(term: Term)(using ectx: EvaluationContext): Option[Term] = // LATER why must this return a Option?
    term match
    case SkelGlue(_, term, _, Skel.Inductive) => Some(term.whnf) // FIXME maybe Unwrap should not be like this
    case Glue(_, term) => Some(term.whnf)
    case _ => Some(term)

object Glued:
  def unapply(term: Term): Option[Term] = // LATER why must this return a Option?
    term match
    case Glue(s, term) => Some(s)
    case _ => Some(term)

extension (term: Term)
  def whnf(using ectx: EvaluationContext): Term =
    term match
    case d: Def => ectx.get(d).get.whnf
    case meta: Meta =>
      ectx.get(meta).map(_.whnf).getOrElse(meta)
    case s: SkelGlue => s // don't reduce on skel glue
    case g: Generic => g
    case eq@Eq(t, a, b) =>
      val reduced = reduceEq(t, a, b)
      if reduced != null then Glue(eq, reduced) else eq
    case App(l, r) =>
      l.whnf match
      case SkelGlue(stt, term, glo, Skel.Lambda(skel1)) =>
        SkelGlue(App(stt, r), App(term, r).whnf, glo, skel1)
      case glue@SkelGlue(stt, term, glo, Skel.PatternLambda(bs)) => 
        constructWhnf(term.asInstanceOf[PatternLambda], r) match
        case (res, index) =>
          bs(index) match
          case skel1: Skel => SkelGlue(App(stt, r), res.whnf, glo, skel1)
          case _ => res.whnf // def removed because no more skel!
        case _ => App(glue, r.whnf)
      case Lambda(body) => body(r).whnf
      case l: PatternLambda =>
        constructWhnf(l, r) match
        case (res, _) => res.whnf
        case _ => App(l, r)
      case ll => App(ll, r)
    case Proj(l, r) =>
      l.whnf match
      case Make(ts) => ts(r).whnf
      case ll => Proj(ll, r)
    case _ => term

def reduceEq(t: Term, a: Term, b: Term)(using ectx: EvaluationContext): Term | Null = // FIXME it is ok to use 0 here?
  t.whnf match
  case Prop(i) => App(App(iff(i), a), b).whnf
  case Set(i) => ???
    // (a.whnf, b.whnf) match
    // case (Set(l1), Set(l2)) => if l1 == l2 then top else bottom
  case Unwrap(Pi(d, c, e)) => Pi(d, (x) => Eq(c(x), App(a, x), App(b, x)), e)
  case Unwrap(Record(tele, e)) =>
    (a.whnf, b.whnf) match
    case (Make(as), Make(bs)) => Record(reduceEqTelescope(tele, as, bs), e)
    case _ => null
  case Unwrap(en@Enum(kases, e)) =>
    (a.whnf, b.whnf) match
    case (Construct(k1, f1), Construct(k2, f2)) =>
      if k1 != k2 then bottom(0) else reduceEq(en(k1), f1, f2)
    case _ => null
  case a: Meta => null
  case a: Generic => null
  case _ => logicError()

def reduceEqTelescope(tele: Telescope, as: Seq[Term], bs: Seq[Term]): Telescope = 
  tele match
  case Telescope.Nil => 
    contract { as.size == 0 && bs.size == 0 }
    Telescope.Nil
  case Telescope.Cons(head, tail) =>
    // FIXME equality
    Telescope.Cons(Eq(head, as.head, bs.head), (a) => Telescope.Nil)
    //Telescope.Cons(Eq(head, as.head, bs.head), ???)

inline def constructWhnf(lambda: PatternLambda, stuck: Term)(using ectx: EvaluationContext): (Term, Int) | Null =
  var res: Term | Null = null
  var index = -1
  var cs = lambda.branches
  while cs.nonEmpty && res == null do
    index += 1
    val c = cs.head
    val ss = stuck.extract(c.pattern)
    res = if ss != null then c.clos(ss) else null
    cs = cs.tail
  if res == null then
    null
  else
    (res, index)

extension (v: Term) def extract(pattern: Pattern)(using ectx: EvaluationContext): Seq[Term] | Null =
  val vs = mutable.ArrayBuffer[Term]()
  def goAll(ps: Seq[Pattern], vs: Seq[Term]): Boolean =
    if ps.size != vs.size then logicError()
    var i = 0
    var ret = true
    while i < ps.length do
      if !go(ps(i), vs(i)) then
        ret = false
        i = ps.length
      i += 1
    return ret

  def go(pattern: Pattern, v: Term): Boolean =
    pattern match
    case Pattern.Generic =>
      vs.append(v); true
    case Pattern.Make(names) =>
      v.whnf match
      case Make(values) => goAll(names, values)
      case _ => false
    case Pattern.Construct(name, pt) =>
      v.whnf match
      case Construct(n, v1) if name == n => go(pt, v1)
      case _ => false

  if go(pattern, v) then vs.toSeq else null