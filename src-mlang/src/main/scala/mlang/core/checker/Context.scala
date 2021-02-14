package mlang.core.checker

import mlang.core._
import utils._
import value._
import translate._

class NotResolvedException() extends Exception

type ExtendHandler[T] = (Term) => Context ?=> T

trait Context extends Env:
  override def apply(r: syntax.Ref): Inversable
  def extend(r: Inversable): Context = new SimpleContext(this, Seq(r))

object EmptyContext extends Context:
  def apply(r: syntax.Ref) = throw NotResolvedException()
  def apply(r: syntax.GlobalRef) = throw NotResolvedException()
  def apply(r: syntax.TempRef) = throw NotResolvedException()

class SimpleContext(base: Context, bs: Seq[Inversable]) extends SimpleEnv(base, bs) with Context:
  override def apply(r: syntax.Ref): Inversable = 
    val i = bs.size - r.get
    if i >= 0 then bs(i) else base(r -bs.size)
  override def extend(r: value.Inversable): Context = SimpleContext(base, bs :+ r)

def generic[T](typ: Term)(handler: ExtendHandler[T])(using ctx: Context): T =
  val term = value.Generic(typ)
  val ctx1 = ctx.extend(term)
  handler(term)(using ctx1)

def meta[T](typ: Term, term: Term)(using ctx: Context): Context =
  val gen = value.Meta(typ, term)
  ctx.extend(gen)

def define(term: Term, typ: Term, global: Boolean, skel: Skel | Null)(using ctx: Context): Context =
  val gen = value.Def(typ, SkelGlue.generic(typ, term, global, skel))
  ctx.extend(gen)
 
def declare(typ: Term)(using ctx: Context): Context =
  val gen = value.Def(typ, value.Generic(typ))
  ctx.extend(gen)

def defrec[T](r: syntax.Ref, skel: Skel | Null, tm: Term)(using ctx: Context): Context = 
  ctx(r) match
  case d: Def =>
    contract { d.term.isInstanceOf[value.Generic] }
    d.term = SkelGlue.generic(d.typ, tm, true, skel)
    ctx
  case _ => logicError()
