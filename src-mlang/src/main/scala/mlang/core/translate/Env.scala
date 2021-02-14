package mlang.core.translate

import mlang.core._
import mlang.core.value._
import mlang.core.syntax.{ Ref, TempRef, GlobalRef }

trait Env:
  def apply(r: TempRef): Meta
  def apply(r: Ref): Term
  def apply(r: GlobalRef): Inversable

  def :+(r: Term): Env = SimpleEnv(this, Seq(r))

class SimpleEnv(val base: Env, val bs: Seq[Term]) extends Env:
  def apply(r: GlobalRef): Inversable = base(r)
  def apply(r: TempRef): Meta = base(r)
  def apply(r: Ref): Term = 
    val i = bs.size - r.get
    if i >= 0 then bs(i) else base(r - bs.size)
  override def :+(r: Term): Env = SimpleEnv(base, bs :+ r)
