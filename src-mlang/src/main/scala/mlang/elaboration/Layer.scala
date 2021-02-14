package mlang.elaboration

import utils._
import mlang.infra._
import mlang.core._
import mlang.core.translate._
import mlang.core.value.{ Def, Meta, Inversable, ConversionChecker, MetaGenericsApps, Generic, SkelGlue, Axiom }
import mlang.core.syntax.dbi.{ shift, lift, InsertInfo }
import scala.collection.mutable
import mlang.core.syntax.skel

sealed abstract class LocalLayer(override val term: Inversable | Null) extends ReifyLayer(term):
  var finished = false
  def name: Name
  def metas = boxes.asInstanceOf[mutable.ArrayBuffer[Meta]]

  override def inverse(v: Inversable): Int =
    if finished then
      if term != null && isInverseOf(v, term) then -1 else -2
    else
      super.inverse(v)
  
  def finish(): Seq[Meta] = 
   // if finished then logicError()
    finished = true
    // cannot clear metas, it is used for eval, but cannot be used by inverse
    val ms = metas.toSeq
    ms

case class DummyLayer() extends LocalLayer(null):
  def name = ""

case class GenericLayer(val name: Name, override val term: Inversable) extends LocalLayer(term):
  override def toString(): String = s"Layer($name, $term, ${metas}"

