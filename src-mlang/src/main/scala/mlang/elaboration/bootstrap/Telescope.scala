package mlang.elaboration.bootstrap

import mlang.infra._
import utils._
import mlang.elaboration._
import mlang.core._
import mlang.core.translate.eval
import mlang.core.syntax.dbi.shift
import dench.bootstrap._


case class NameTyp(name: PlicitName, typ: Term)

// FEATURE proper support this feature, this causes wrong kind of name collison like : `a b: a`
extension (t: Tele) def nameTyps: Seq[NameTyp] = 
  t.flatMap(_ match {
    case NamesTyp(ns, t) => 
      if ns.isEmpty then Seq(NameTyp("", t))
      else ns.map(n => NameTyp(n, t))
    case _ => logicError()
  })

extension (t: Tele) def nameTypDefs: Seq[NameTyp | Def] = 
  t.flatMap(_ match {
    case NamesTyp(ns, t) => 
      if ns.isEmpty then Seq(NameTyp("", t))
      else ns.map(n => NameTyp(n, t))
    case d: Def => Seq(d)
    case _ => logicError()
  })

def inferTelescope(tele: Tele)(using Context): (syntax.Telescope, Seq[etyp.Field], Sort) =
  inferTelescopeInner(tele.nameTypDefs)

def inferTelescopeInner(tele: Seq[NameTyp | Def] )(using Context): (syntax.Telescope, Seq[etyp.Field], Sort) =
  tele match
  case h +: tail =>
    h match
    case NameTyp(name, t) =>
      val (ts, tt) = t.inferAsTyp
      val (ms, (t1, fs1, s1)) = generic(name.unwrap, tt.eval) { _ => inferTelescopeInner(tail) }
      (
        syntax.Telescope.Cons(ts, syntax.TelescopeClosure(t1.shift(0, ms, null))),
        name +: fs1,
        Sort.lub(s1, tt)
      )
    case d: Def =>
      val (ms, ctx1) = checkLocalBox(d)
      val (t1, fs1, s1) = inferTelescopeInner(tail)(using ctx1)
      (syntax.Telescope.Let(Seq(ms), t1), fs1, s1)
  case _ => (syntax.Telescope.Nil, Seq.empty, Prop.Zero)

def checkByTelescope(tele0: value.Telescope, fields0: Seq[etyp.Field], vs: Seq[PlicitTerm])(using Context): Seq[syntax.Term] =
  val (tele, fields, adds) = if vs.size == 0 then (tele0, fields0, Seq.empty) else insertTelescopeImplicitApps(tele0, fields0, vs.head.plicity)
  tele match
  case value.Telescope.Cons(h, t) =>
    vs match
    case (v: PlicitTerm) +: tail =>
      if v.plicity == fields.head.plicity then
        val v_ = v.unwrap.check(h)
        (adds :+ v_) ++ checkByTelescope(t(v_.eval), fields.tail, tail)
      else notHandled()
    case _ => notHandled()
  case value.Telescope.Nil => 
    if vs.isEmpty then adds else notHandled()


def insertTelescopeImplicitApps(
  tele: value.Telescope,
  fields: Seq[etyp.Field],
  until: Plicity,
  add: Seq[syntax.Term] = Seq.empty)(using ctx: Context): (value.Telescope, Seq[etyp.Field], Seq[syntax.Term]) =
  if fields.size > 0 && fields.head.plicity != until && fields.head.plicity.impli then
    tele match
    case value.Telescope.Cons(h, t) =>
      trace.log(s"instinated telescope meta")
      val s = ctx.meta(h)
      insertTelescopeImplicitApps(t(s.eval), fields.tail, until, add :+ s)
    case _ => logicError()
  else (tele, fields, add)