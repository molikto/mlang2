package mlang.elaboration

import utils._
import mlang.infra._
import mlang.core._
import mlang.core.translate._
import mlang.core.value.{ Def, Meta, Inversable, ConversionChecker, MetaGenericsApps, Generic, SkelGlue, Axiom }
import mlang.core.syntax.dbi.{ shift, lift, InsertInfo, findRef }
import scala.collection.mutable
import mlang.core.syntax.skel


trait GlobalLayer:
  def inverse(v: Inversable): Int
  def ref: Ref

class DeclLayer(val ref: Ref, val term: Def) extends GlobalLayer:
  def inverse(v: Inversable): Int = if isInverseOf(v, term) then 0 else -1
  
// FIXME this definition is very very recursive/mutable, can we do better?
case class DefLayer(var ctx: Context, val ref: Ref, typ: syntax.Term, term: syntax.Term | Null) extends GlobalLayer:
  val rec = if term == null then false else term.findRef(ref)
  val skel = if term != null then term.skel(rec) else null // FIXME proper rec parmaeter for skel
  override def toString(): String = "def"
  val lifts = mutable.ArrayBuffer[value.Inversable | Null]()
  def inverse(v: Inversable): Int =
    var i = 0
    var ret = -1
    while ret == -1 && i < lifts.size do
      val vv = lifts(i)
      if vv != null && isInverseOf(v, vv) then
        ret = i
      i += 1
    ret

  def apply(i: Int): Inversable =
    while lifts.size <= i do
      lifts += null
    if lifts(i) == null then 
      if term == null then
        lifts(i) = value.Axiom(typ.lift(i).eval(using ctx))
      else
        val tp = typ.lift(i).eval(using ctx)
        val df = value.Def(tp, null)
        lifts(i) = df
        df.term = SkelGlue.generic(tp, term.lift(i).eval(using ctx), true, skel)
    lifts(i).nn