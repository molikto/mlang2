package mlang.elaboration.bootstrap

import utils._
import mlang.infra._
import dench.bootstrap._
import mlang.elaboration._
import mlang.core.{ Pattern => P, _ }
import mlang.core.value.whnf
import mlang.core.syntax.dbi.shift
import mlang.core.syntax.dbi.InsertInfo
import mlang.core.translate.reify

given elabLambda: CheckOnlyElaborator[Lambda] with
  extension (thiz: Lambda) def check(typ: value.Term)(using Context): syntax.Term =
    if thiz.branches.map(a => a.params.size).toSet.size > 1 then notHandled()
    checkLambda(thiz, typ)
  // MAYBE this case not supported due to no level variables https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L113

private def checkBody(bd: Branch, typ: value.Term)(using Context): syntax.Term =
  if bd.params.size == 0 then bd.clos.check(typ)
  else checkLambda(Lambda(Seq(bd)), typ)
private def checkLambda(lam: Lambda, typ: value.Term)(using old: Context): syntax.Term =
  // FIXME also our multi arg pattern seems very wrong
  val pts0 = lam.branches.map(a => (a.params.head, Branch(a.params.tail, a.clos)))
  val plicity =
    if pts0.size == 0 then Plicity.Ex else
    if pts0.forall(_._1.plicity == pts0(0)._1.plicity) then pts0(0)._1.plicity
    else notHandled()
  val pts = pts0.map(a => (a._1.unwrap, a._2))
  typ.whnf match
  case value.Unwrap(value.Pi(a, b, e)) =>
    def patternLambda() = {
      val bs = pts.map(branch => {
        trace.enter("pattern lambda")
        val (p, ms, syn: syntax.Term) = declarePattern(branch._1, a) { gen => 
          trace.enter("pattern lambda body")
          val ret = if hasGeneric(gen) then
            checkBody(branch._2, b(gen))
          else // we do this so metas is always introduced in pattern lambda branches... part of the reason is elab match expression to pattern lambda with proper skel... sad
            val (ms, res) = dummyGeneric() { _ => checkBody(branch._2, b(gen)) }
            res.shift(-1, ms)
          trace.exit(s"pattern lambda body $ret")
          ret
        }
        trace.exit(s"pattern lamdba end $ms, $syn")
        syntax.Branch(p, syntax.Closure(syn.shift(0, ms)))
      })
      syntax.PatternLambda(bs)
    }
    def simpleLambda(name: Name, body: Branch)(using Context) = {
      val (ms, body_) = generic(name, a) { gen => checkBody(body, b(gen)) }
      syntax.Lambda(syntax.Closure(body_.shift(0, ms)))
    }
    if e.name.plicity == plicity then
      pts match
      case Seq((name, body)) =>
        name match
        case n: Name => simpleLambda(n, body)
        case _ => patternLambda()
      case _ => patternLambda()
    else if e.name.plicity.impli then
      checkLambda(Lambda(Seq(Branch(Seq(Plicit("", e.name.plicity)), lam))), typ)
    else notHandled()
  case a =>
    println(a)
    notHandled()

private def hasGeneric(v: value.Term): Boolean =
  v match
  case value.Make(vs) => vs.exists(hasGeneric)
  case value.Construct(_, vs) => vs.exists(hasGeneric)
  case _: value.Generic => true
  case _ => false

private def declareTelescope[T](
  patterns: Seq[PlicitPattern],
  fs: value.Telescope,
  et: Seq[etyp.Field],
  vmapper: Seq[value.Term] => value.Term,
  vs: Seq[value.Term] = Seq.empty
)(handler: ExtendHandler[T])(using Context): (Seq[P], InsertInfo | Null, T) = 
  trace.enter(s"declare pattern tele $patterns")
  val res: (Seq[P], InsertInfo | Null, T) = fs match
  case value.Telescope.Cons(h, tail) =>
    val pattern = patterns.head
    val (p1, m1, (p2, m2, t)) = if et.head.plicity == pattern.plicity then
      declarePattern(pattern.unwrap, h) { gen =>
        declareTelescope(patterns.tail, tail(gen), et.tail, vmapper, vs :+ gen)(handler)
      }
    else if et.head.plicity.impli then
      generic("", h) { gen => 
        declareTelescope(patterns, tail(gen), et.tail, vmapper, vs :+ gen)(handler)
      }
    else 
      notHandled()
    contract { m2 == null || m1 == null }
    (p1 +: p2, if m1 == null then m2 else m1, t)
  case value.Telescope.Nil => 
    (Seq.empty, null, handler(vmapper(vs)))
  trace.exit()
  res


private def declarePattern[T](pattern: Pattern, typ: value.Term)(handler: ExtendHandler[T])(using Context): (P, InsertInfo | Null, T) =
  trace.enter(s"declare pattern $pattern")
  val res: (P, InsertInfo | Null, T) = pattern match
  case name: Name =>
    def fallback() =
      val (ms, t) = generic(name, typ)(handler)
      (P.Generic, ms, t)
    if name != "" then
      typ.whnf match
      case value.Unwrap(value.Enum(_, e)) =>
        e(name) match
        case (index, k) if k.fields.size == 0 =>
          (P.Construct(index, Seq.empty), null, handler(value.Construct(index, Seq.empty)))
        case _ => fallback()
      case _ => fallback()
    else fallback()
  case MakePattern(fields) =>
    typ.whnf match
    case value.Unwrap(value.Record(fs, e)) =>
      if e.fields.size != fields.size then
        notHandled()
      else
        val (ps, ms, t) = declareTelescope(fields, fs, e.fields, vs => value.Make(vs))(handler)
        (P.Make(ps), ms, t)
    case _ => notHandled()
  case ConstructPattern(kase, fields) =>
    typ.whnf match
    case value.Unwrap(value.Enum(kases, et)) =>
      et(kase) match
      case (index, k) =>
        if k.fields.size != fields.size then
          notHandled()
        else
          val (ps, ms, t) = declareTelescope(fields, kases(index), k.fields, vs => value.Construct(index, vs))(handler)
          (P.Construct(index, ps), ms, t)
      case null => notHandled()
    case _ => notHandled()
  trace.exit()
  res
