package mlang.core.translate

import utils.{ logicError, contract }
import mlang.core._
import mlang.core.value._
import mlang.core.syntax.dbi._
import mlang.core.syntax.dbi.shift
import scala.collection.mutable

def isInverseOf(v: Inversable, term: Term) = 
  term match
  case Def(_, s: SkelGlue) if s.stuck.eq(v) => true
  case t => t.eq(v)

class ReifyLayer(val term: Term | Null):
  val boxes: mutable.ArrayBuffer[Inversable] = mutable.ArrayBuffer()
  def inverse(v: Inversable): Int =
    if term != null && isInverseOf(v, term) then -1 else
      val index = boxes.indexWhere(a => a.eq(v))
      if index >= 0 then index else -2

trait ReifyContext:
  def size: Int
  def inverse(v: Inversable): syntax.RefLike | Null

extension (v: Term) def reify(using env: ReifyContext) = internal.reify0(v, env)

package internal:

  def fgen = Generic(Prop.Zero)
       
  private [core] def reify0(v: Term, env: ReifyContext): syntax.Term = 
    val ctx = ReifyCtx(env, Seq(ReifyLayer(fgen)))
    val res = v.reify(using ctx)
    val boxes = ctx.finish().zipWithIndex.map(p => p._1.shiftWithDepth(-1, p._2))
    res.shift(-1, InsertInfo(ctx.size - 1, (0 until boxes.size).toSeq, boxes)) match // LATER less shift?
    case syntax.Let(Seq(), a) => a
    case a => a

  private class ReifyCtx(val base: ReifyContext, val layers: Seq[ReifyLayer]) extends ReifyContext:
    val size = base.size + layers.size

    def inverse(v: Inversable): syntax.RefLike | Null =
      var ret: syntax.RefLike | Null = null
      var i = layers.size
      while ret == null && i > 0 do
        i -= 1
        val l = layers(i)
        val res = l.inverse(v)
        if res == -1 then
          ret = syntax.Ref(layers.size - i)
        else if res >= 0 then
          ret = syntax.TempRef(base.size + i, res)
      if ret != null then
        ret
      else
        base.inverse(v) match
        case r: syntax.Ref => r + layers.size
        case m => m

    def extend(term: Term): ReifyCtx = ReifyCtx(base, layers :+ ReifyLayer(term))
    def extend(gss: Seq[Term]): ReifyCtx = gss.foldLeft(this: ReifyCtx) { (c, g) => c.extend(g) }

    def inverseOrSave(v: Box): syntax.RefLike =
      val iv = inverse(v)
      if iv != null then iv else
        v.typ.deps(using this)
        v match
        case b: Def => b.term.asInstanceOf[Term].deps(using this)
        case Meta(_, term) => if term == null then logicError() else term.deps(using this)
        val ret = syntax.TempRef(base.size + layers.size - 1, layers.last.boxes.size)
        layers.last.boxes += v
        ret // TODO if all dependency is not in current layer, move layer bellow
    
    def finish(): Seq[syntax.Box.Local] =
      var ctx: ReifyCtx = this
      val boxes = layers.last.boxes
      boxes.toSeq.map(v => {
        val ret = v match
        case Def(tp, tm: Term) => syntax.Box.Def(tp.reify(using ctx), tm.reify(using ctx))
        case Meta(tp, tm: Term) => syntax.Box.Meta(tp.reify(using ctx), tm.reify(using ctx))
        case _ => logicError()
        ctx = ctx.extend(v)
        ret
      })


  extension (v: Term) private def reify(using ctx: ReifyCtx): syntax.Term =
    v match
    case Pi(d, c, e) => syntax.Pi(d.reify, c.reifyC, e)
    case Eq(t, a, b) => syntax.Eq(t.reify, a.reify, b.reify)
    case Record(fs, e) => syntax.Record(fs.reify, e)
    case Enum(ks, e) => syntax.Enum(ks.map(_.reify), e)
    case Lambda(b) => syntax.Lambda(b.reifyC)
    case Make(fs) => syntax.Make(fs.map(_.reify))
    case Construct(k, fs) => syntax.Construct(k, fs.map(_.reify))
    case App(l, r) => syntax.App(l.reify, r.reify)
    case Proj(l, r) => syntax.Proj(l.reify, r)
    case PatternLambda(bs) => syntax.PatternLambda(bs.map(b => syntax.Branch(b.pattern, b.clos.reifyC)))
    case Glue(l, _) => l.reify
    case s: Sort => s
    case g: Generic => { val r = ctx.inverse(g); if r == null then logicError() else r }
    case a: Axiom => { val r = ctx.inverse(a); if r == null then logicError() else r }
    case d: Def => ctx.inverseOrSave(d)
    case m: Meta => ctx.inverseOrSave(m)
    case k: SkelGlue =>
      k.stuck match
      case GenericApps(h, s) if ctx.inverse(h) != null => k.stuck.reify
      case _ => k.reduced.reify

  extension (fs: Telescope) private def reify(using ReifyCtx): syntax.Telescope = 
    fs match
    case Telescope.Cons(h, t) => syntax.Telescope.Cons(h.reify, t.reifyC)
    case Telescope.Nil => syntax.Telescope.Nil

  extension (closure: TelescopeClosure) private def reifyC(using ctx: ReifyCtx): syntax.TelescopeClosure =
    val g = fgen
    val ctx1 = ctx.extend(g)
    val res = closure(g).reify(using ctx1)
    val boxes = ctx1.finish()
    syntax.TelescopeClosure(res.shift(0, InsertInfo(ctx1.size - 1, (0 until boxes.size).toSeq, boxes), null))

  extension (closure: Closure) private def reifyC(using ctx: ReifyCtx): syntax.Closure = 
    closure match
    case c: Term => syntax.Closure(c.reify)
    case _ => 
      val (ctx1, res) = closure match
      case c: Closure1 =>
        val gs = (0 until 1).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0)).reify(using ctx1)
        (ctx1, res)
      case ErrorClosure =>
        (ctx, syntax.Error)
      case c: Closure2 =>
        val gs = (0 until 2).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0), gs(1)).reify(using ctx1)
        (ctx1, res)
      case c: Closure3 =>
        val gs = (0 until 3).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0), gs(1), gs(2)).reify(using ctx1)
        (ctx1, res)
      case c: Closure4 =>
        val gs = (0 until 4).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0), gs(1), gs(2), gs(3)).reify(using ctx1)
        (ctx1, res)
      case c: Closure5 =>
        val gs = (0 until 5).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0), gs(1), gs(2), gs(3), gs(4)).reify(using ctx1)
        (ctx1, res)
      case c: Closure6 =>
        val gs = (0 until 6).map(_ => fgen)
        val ctx1 = ctx.extend(gs)
        val res = c(gs(0), gs(1), gs(2), gs(3), gs(4), gs(5)).reify(using ctx1)
        (ctx1, res)
      case _: Term => logicError()
      val boxes = ctx1.finish()
      syntax.Closure(res.shift(0, InsertInfo(ctx1.size - 1, (0 until boxes.size).toSeq, boxes)))
    

  extension (v: Term) private def deps(using ctx: ReifyCtx): Unit =
    v match
    case Pi(d, c, _) => d.deps; c.depsC
    case Eq(t, a, b) => t.deps; a.deps; b.deps
    case Record(fs, _) => fs.deps
    case Enum(ks, _) => ks.foreach(_.deps)
    case Lambda(b) => b.depsC
    case Make(fs) => fs.map(_.deps)
    case Construct(k, fs) => fs.foreach(_.deps)
    case App(l, r) => l.deps; r.deps
    case Proj(l, _) => l.deps
    case PatternLambda(bs) => bs.foreach(b => b.clos.depsC)
    case d: Def => ctx.inverseOrSave(d)
    case m: Meta => ctx.inverseOrSave(m)
    case Glue(l, _) => l.deps
    case k: SkelGlue =>
      k.stuck match
      case GenericApps(h, s) if ctx.inverse(h) != null => k.stuck.deps
      case _ => k.reduced.deps
    case s: Sort =>
    case g: Generic =>
    case g: Axiom =>

  extension (fs: Telescope) private def deps(using ReifyCtx): Unit = 
    fs match
    case Telescope.Cons(h, t) => h.deps; t.depsC
    case Telescope.Nil => 
  
  extension (fs: TelescopeClosure) private def depsC(using ReifyCtx): Unit = 
    ()

  extension (closure: Closure) private def depsC(using ctx: ReifyCtx): Unit = 
    () // TODO we don't track dependencies in closures now, this will result in duplicated dependency but ok~
    // val g = Generic(Prop.Zero) // ok to use one~
    // closure match
    // case c: Term => c.deps
    // case c: Closure1 => c(g).deps
    // case c: Closure2 => c(g, g).deps
    // case c: Closure3 => c(g, g, g).deps
    // case c: Closure4 => c(g, g, g, g).deps
    // case c: Closure5 => c(g, g, g, g, g).deps
    // case c: Closure6 => c(g, g, g, g, g, g).deps