package mlang.core.translate

import utils.{ logicError, contract }
import mlang.core._
import value._
import mlang.core.syntax.skel



extension (syn: syntax.Term)
  def eval(using env: Env): Term =
    syn match
    case a: Sort => a
    case ref: syntax.Ref => env(ref)
    case ref: syntax.TempRef => env(ref)
    case ref: syntax.GlobalRef => env(ref)
    case syntax.Pi(a, b, e) => Pi(a.eval, b.eval1, e)
    case syntax.Eq(t, a, b) => Eq(t.eval, a.eval, b.eval)
    case syntax.Lambda(body) => Lambda(body.eval1)
    case syntax.App(a, b) => App(a.eval, b.eval)
    case syntax.Record(fields, e) => Record(fields.eval, e)
    case syntax.Make(ts) => Make(ts.map(_.eval))
    case syntax.Proj(v, i) => Proj(v.eval, i)
    case syntax.Enum(kases, e) => Enum(kases.map(_.eval), e)
    case syntax.Construct(b, t) => Construct(b, t.eval)
    case syntax.PatternLambda(bs) => PatternLambda(bs.map(c => Branch(c.pattern, c.clos.eval(c.pattern.size))))
    case syntax.Let(ds, in) => evalLet(ds, in)
    case syntax.Error => throw EvalErrorException()

private def evalDefs(ds: Seq[syntax.Box.Local])(using env: Env): Env =
  var ev = env 
  ds.foreach(d => {
    d match
    case syntax.Box.Def(typ, tm) =>
      val tp = typ.eval(using ev)
      ev = ev :+ Def(tp, SkelGlue.generic(tp, tm.eval(using ev), false, tm.skel(false, valid = true)))
    case syntax.Box.Meta(typ, tm) =>
      ev = ev :+ Meta(typ.eval(using ev), tm.eval(using ev))
  })
  ev

private def evalLet(ds: Seq[syntax.Box.Local], in: syntax.Term)(using env: Env): Term =
  val ev = evalDefs(ds)
  in.eval(using ev)


extension (t: syntax.Telescope) private def eval(using env: Env): Telescope =
  t match
  case syntax.Telescope.Nil => Telescope.Nil
  case syntax.Telescope.Cons(h, tail) =>
    val hh = h.eval
    Telescope.Cons(hh, SyntaxTelescopeClosure(env, tail.get))
  case syntax.Telescope.Let(defs, tail) =>
    val ev = evalDefs(defs)
    tail.eval(using ev)

private class SyntaxTelescopeClosure(env: Env, clos: syntax.Telescope) extends TelescopeClosure:
  def apply(v: Term): Telescope =
    var ev = env 
    ev = ev :+ v
    clos.eval(using ev)

extension (b: syntax.Closure)
  private def eval1(using env: Env): Closure1 = SyntaxClosure1(env, b.get)
  private def eval2(using env: Env): Closure2 = SyntaxClosure2(env, b.get)
  private def eval3(using env: Env): Closure3 = SyntaxClosure3(env, b.get)
  private def eval4(using env: Env): Closure4 = SyntaxClosure4(env, b.get)
  private def eval5(using env: Env): Closure5 = SyntaxClosure5(env, b.get)
  private def eval6(using env: Env): Closure6 = SyntaxClosure6(env, b.get)
  private def eval(n: Int)(using env: Env): Closure =
    n match
    case 0 => b.get match
      case syntax.Error => ErrorClosure
      case a => a.eval
    case 1 => b.eval1
    case 2 => b.eval2
    case 3 => b.eval3
    case 4 => b.eval4
    case 5 => b.eval5
    case 6 => b.eval6

private class SyntaxClosure1(env: Env, clos: syntax.Term) extends Closure1:
  def apply(v: Term): Term =
    var ev = env 
    ev = ev :+ v
    clos.eval(using ev)


private class SyntaxClosure2(env: Env, clos: syntax.Term) extends Closure2:
  def apply(v0: Term, v1: Term): Term =
    var ev = env 
    ev = ev :+ v0 :+ v1
    clos.eval(using ev)

private class SyntaxClosure3(env: Env, clos: syntax.Term) extends Closure3:
  def apply(v0: Term, v1: Term, v2: Term): Term =
    var ev = env 
    ev = ev :+ v0 :+ v1 :+ v2
    clos.eval(using ev)

private class SyntaxClosure4(env: Env, clos: syntax.Term) extends Closure4:
  def apply(v0: Term, v1: Term, v2: Term, v3: Term): Term =
    var ev = env 
    ev = ev :+ v0 :+ v1 :+ v2 :+ v3
    clos.eval(using ev)

private class SyntaxClosure5(env: Env, clos: syntax.Term) extends Closure5:
  def apply(v0: Term, v1: Term, v2: Term, v3: Term, v4: Term): Term =
    var ev = env 
    ev = ev :+ v0 :+ v1 :+ v2 :+ v3 :+ v4
    clos.eval(using ev)

private class SyntaxClosure6(env: Env, clos: syntax.Term) extends Closure6:
  def apply(v0: Term, v1: Term, v2: Term, v3: Term, v4: Term, v5: Term): Term =
    var ev = env 
    ev = ev :+ v0 :+ v1 :+ v2 :+ v3 :+ v4 :+ v5
    clos.eval(using ev)