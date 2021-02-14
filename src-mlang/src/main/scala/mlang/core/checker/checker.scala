package mlang.core.checker

import mlang.infra._
import mlang.core.translate.eval
import mlang.core._
import mlang.core.syntax.skel
import mlang.core.syntax._
import mlang.core.value.conversion
import mlang.core.value.apply
import mlang.core.value.whnf
import utils.{ logicError, contract }

class CheckFailedException() extends Exception()
class ConversionFailedException() extends Exception()
def checkFailed() = throw new CheckFailedException()

/**
  * checks a program of core syntax
  */
extension (program: Program) def check(): Unit =
  inferDefs(program, true)(using SimpleContext(EmptyContext, Seq.empty))

extension (term: Term)
  def check(typ: value.Term)(using ctx: Context): Unit =
    trace.enter(s"core checker $term $typ")
    val res = (term, typ.whnf) match
    case (Lambda(c), value.Unwrap(value.Pi(a, b, _))) =>
      generic(a) { gen => c.get.check(b(gen)) }
    case (PatternLambda(cs), value.Unwrap(value.Pi(a, b, _))) =>
      cs.foreach(c => declarePattern(c.pattern, a) { gen => c.clos.get.check(b(gen)) })
    case (Make(ts), value.Unwrap(value.Record(fields, _))) =>
      checkByTelescope(fields, ts)
    case (Construct(n, vs), value.Unwrap(value.Enum(ds, _))) =>
      if n < ds.length then checkByTelescope(ds(n), vs) else checkFailed()
    case (Let(ds, in), t) =>
      val ctx1 = inferDefs(ds)
      in.check(typ)(using ctx1)
    case _ =>
      // println("core check term " + term)
      // println("core check typ " + typ)
      val ifd = term.infer
      // println("core check res1 " +  ifd)
      if !conversion.subtyp(ifd, typ) then
        throw ConversionFailedException()
    trace.exit()
    res

  def inferAsTyp(using Context): Sort =
    term.infer match
    case sort: Sort => sort
    case _ => checkFailed()
  
  def inferAsTypThenEval(using Context): value.Term =
    term.inferAsTyp
    term.eval

  def checkThenEval(typ: value.Term)(using Context): value.Term =
    term.check(typ)
    term.eval

  def infer(using ctx: Context): value.Term =
    trace.enter(s"core infer $term")
    val res = term match
    case r: Ref => ctx(r).typ
    case r: TempRef => ctx(r).typ
    case r: GlobalRef => ctx(r).typ
    case s: Sort => s.sort
    case Pi(a, b, e) =>
      val sa = a.inferAsTyp
      Sort.pi(sa, generic(a.eval) { gen => b.get.inferAsTyp })
    case Record(fields, e) => inferTelescope(fields)
    case Enum(ds, e) => Sort.ind(ds.map(inferTelescope))
    case App(l, r) =>
      l.infer.whnf match
      case value.Unwrap(value.Pi(a, b, _)) => r.check(a); b(r.eval)
      case a => checkFailed()
    case Proj(a, b) =>
      a.infer.whnf match
      case value.Unwrap(value.Record(fields, _)) => fields.apply(a.eval, b)
      case _ => checkFailed()
    case Let(ds, in) => 
      val ctx1 = inferDefs(ds)
      in.infer(using ctx1)
    case Eq(a, b, c) =>
      val t = a.infer
      val e = a.eval
      b.check(e)
      c.check(e)
      Prop(t.asInstanceOf[Sort].l)
    case Error => logicError()
    case a => checkFailed()
    trace.exit(res.toString)
    res

private def declareTelescope[T](
  patterns: Seq[Pattern],
  fs: value.Telescope,
  vmapper: Seq[value.Term] => value.Term,
  vs: Seq[value.Term] = Seq.empty
)(handler: ExtendHandler[T])(using Context): T = 
  fs match
  case value.Telescope.Cons(h, tail) =>
    patterns match
    case pattern +: ptail =>
      declarePattern(pattern, h) { gen =>
        declareTelescope(ptail, tail(gen), vmapper, vs :+ gen)(handler)
      }
    case _ => checkFailed()
  case value.Telescope.Nil => 
    if patterns.size == 0 then handler(vmapper(vs)) else checkFailed()

private def declarePattern[T](pattern: Pattern, typ: value.Term)(handler: ExtendHandler[T])(using Context): T =
  pattern match
  case Pattern.Generic =>
    generic(typ)(handler)
  case Pattern.Make(fields) =>
    typ.whnf match
    case value.Unwrap(value.Record(fs, _)) =>
      declareTelescope(fields, fs, vs => value.Make(vs))(handler)
    case _ => checkFailed()
  case Pattern.Construct(index, fields) =>
    typ.whnf match
    case value.Unwrap(value.Enum(kases, _)) =>
      if index < kases.size then
        val k = kases(index)
        declareTelescope(fields, kases(index), vs => value.Construct(index, vs))(handler)
      else checkFailed()
    case _ => checkFailed()

private def inferDef[T](h: Box, global: Boolean)(using ctx: Context): Context =
  // println(s"infer def $h")
  h match
  case Box.Decl(typ) =>
    contract { global }
    val p = typ.inferAsTypThenEval
    declare(p)
  case Box.DefRec(ref, term) =>
    val decl = ctx(ref).typ
    val tm = term.checkThenEval(decl)
    defrec(ref, term.skel(true), tm)
  case Box.Def(typ, term) =>
    val p = typ.inferAsTypThenEval
    val t = term.checkThenEval(p)
    define(t, p, global, term.skel(false))
  case Box.Meta(typ, term) =>
    val p = typ.inferAsTypThenEval
    val t = term.checkThenEval(p)
    meta(p, t)

private def inferDefs[T](defs: Seq[Box], global: Boolean = false)(using ctx: Context): Context = 
  var ctx1 = ctx
  defs.foreach(d => {
    ctx1 = inferDef(d, global)(using ctx1)
  })
  return ctx1

private def inferTelescope(tele: Telescope)(using Context): Sort =
  tele match
  case Telescope.Nil => Prop.Zero
  case Telescope.Let(defs, in) =>
    val ctx = inferDefs(defs)
    inferTelescope(in)(using ctx)
  case Telescope.Cons(h, in) => 
    val tt = h.inferAsTyp
    val s = generic(tt.eval) { _ => inferTelescope(in.get) }
    Sort.lub(s, tt)

private def checkByTelescope(tele: value.Telescope, vs: Seq[Term])(using Context): Unit =
  tele match
  case value.Telescope.Nil => 
    if vs.nonEmpty then checkFailed()
  case value.Telescope.Cons(h, t) =>
    vs match
    case v +: tail =>
      v.check(h)
      checkByTelescope(t(v.eval), tail)
    case _ => checkFailed()
