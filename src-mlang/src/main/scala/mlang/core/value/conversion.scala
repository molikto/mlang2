package mlang.core.value

import mlang.infra._
import mlang.core._
import utils._

import scala.language.implicitConversions


val FEATURE_SUBTYPE = true

private implicit def optToBool(x: Term | Null): Boolean = x != null

// FIXME is this correct?? A Typ-Checking Algorithm for Martin-L¨of Typ Theory with Subtyping Based on Normalisation by Evaluation
private def choose[T](d1: T, d2: T, mode: Int): T = if mode < 0 then d1 else d2

val conversion = ConversionChecker()

class ConversionChecker:
  def subtyp(t1: Term, t2: Term): Boolean =
    subtyp(t1, t2, if (FEATURE_SUBTYPE) -1 else 0)

  private def subtyp(a: Term, t1: Closure1, t2: Closure1, dir: Int): Boolean =
    val gen = Generic(a)
    subtyp(t1(gen), t2(gen), dir)

  private def subtyp(tt1: Telescope, tt2: Telescope, dir: Int): Boolean =
    (tt1, tt2) match
    case (Telescope.Cons(h1, t1), Telescope.Cons(h2, t2)) => 
      subtyp(h1, h2, dir) && { val d = choose(h1, h2, dir); subtyp(t1(d), t2(d), dir) }
    case (Telescope.Nil, Telescope.Nil) => true
    case _ => false

  private def subtyp(t1: Term, t2: Term, dir: Int, glue: Boolean = true): Boolean =
    val res: Boolean = if t1.eq(t2) then true else
      (t1.whnf, t2.whnf) match
      case (s1: Sort, s2: Sort) =>
        Sort.subsort(s1, s2, dir)
      case (Glued(Eq(t1, a1, b1)), Glued(Eq(t2, a2, b2))) if glue =>
        val t = choose(t1, t2, dir)
        val firstTry = subtyp(t1, t2, dir) && equals(t, a1, a2) && equals(t, b1, b2) 
        firstTry || (glue && subtyp(t1, t2, dir, false))
      case (SkelGlue(gen1, _, glo1, Skel.Inductive), SkelGlue(gen2, _, glo2, Skel.Inductive)) =>
        contract { glo1 && glo2 }
        equalsStuck(gen1, gen2)
      case (Unwrap(Pi(a1, b1, e1)), Unwrap(Pi(a2, b2, e2))) =>
        e1 ~ e2 && subtyp(a1, a2, -dir) && subtyp(choose(a1, a2, -dir), b1, b2, dir)
      case (Unwrap(Record(f1, e1)), Unwrap(Record(f2, e2))) =>
        e1 ~ e2 && subtyp(f1, f2, dir)
      case (Unwrap(Enum(b1, e1)), Unwrap(Enum(b2, e2))) =>
        e1 ~ e2 && b1.zip(b2).forall(pair => subtyp(pair._1, pair._2, dir))
      case (t1, t2) =>
        equalsStuck(t1, t2)
    if !res then
      trace.log(s"subtyp failed ${Unwrap.unapply(t1.whnf)}, ${Unwrap.unapply(t2.whnf)}")
    res

  private def equalsStuck(t1: Term, t2: Term): Term | Null = 
    equalsStuckWhnf(t1.whnf, t2.whnf)

  private def refEq(a: Term, b: Term): Term | Null = 
    if a.eq(b) then a.typ else null

  protected def equalsStuckWhnf(w1: Term, w2: Term): Term | Null = 
    val res: Term | Null = (w1, w2) match
    case (g1: Axiom, g2: Axiom) => refEq(g1, g2)
    case (g1: Generic, g2: Generic) => refEq(g1, g2)
    case (App(l1, a1), App(l2, a2)) =>
      val res = equalsStuck(l1, l2)
      if res == null then null else
        res.whnf match
        case Pi(a, b, _) =>
          if equals(a, a1, a2) then b(a1) else null
        case _ => logicError()
    case (SkelGlue(stk1, rdc1, glo1, Skel.PatternLambda(_)), SkelGlue(stk2, rdc2, glo2, Skel.PatternLambda(_))) =>
      if glo1 != glo2 then null else
        // globally defined lambda, might be recursive
        // but because global items of head generic always have same env
        // we can check the stuck term for equals checking
        // and no need to go inside the lambda body,
        // which we cannot do because this might cause infinite loop
        if glo1 then
          equalsStuck(stk1, stk2)
        // locally defined pattern lambda, not recursive,
        // but we should check the body for equals:
        // checking stuck term is not complete, because local generic is generated new each evaluation
        // checking reduced term is ok, because they are restricted to be non-recursive
        else
          ??? // TODO implement local pattern lambda equals checking
    case (Proj(m1, f1), Proj(m2, f2)) =>
      if f1 != f2 then null else
        val res = equalsStuck(m1, m2)
        if res == null then null else
          res.whnf match
          case Unwrap(Record(fields, _)) => fields(m1, f1)
          case _ => logicError()
    // all pattern lambdas must be guarded by SkelGlue, by SkelChecker TODO: transparent pattern lambdas (for records)
    case (p: PatternLambda, _) => logicError()
    case (_, p: PatternLambda) => logicError()
    case (a, b) =>
      null
    if res == null then
      trace.log(s"equals whnf failed $w1 " + w2.whnf)
    res

  private def equals(tele: Telescope, base: Int, v1: Int => Term, v2: Int => Term): Boolean =
    tele match
    case Telescope.Cons(h, t) =>
      val a = v1(base)
      equals(h, a, v2(base)) && equals(t(a), base + 1, v1, v2)
    case Telescope.Nil => true

  private def equals(typ: Term, v1: Term, v2: Term): Boolean =
    trace.enter(s"equals ${v1.whnf}, ${v2.whnf}")
    val ret: Boolean = if v1.eq(v2) then true else
      val tw = typ.whnf
      tw.sort match
      case Prop(_) => true
      case _ =>
        tw match
        case Pi(a, b, _) =>
          val c = Generic(a)
          equals(b(c), App(v1, c), App(v2, c))
        case Unwrap(r: Record) =>
          equals(r.fields, 0, i => Proj(v1, i), i => Proj(v2, i))
        case Unwrap(s: Enum) =>
          (v1.whnf, v2.whnf) match
          case (Construct(b1, v1), Construct(b2, v2)) =>
            b1 == b2 && equals(s(b1), v1, v2)
          case _ => equalsStuck(v1, v2)
        case u: Sort => subtyp(v1, v2, 0)
        case _ => equalsStuck(v1, v2)
    trace.exit(ret.toString)
    ret