package mlang.elaboration.bootstrap

import utils._
import mlang.elaboration._
import mlang.core._
import mlang.core.translate.eval
import mlang.core.syntax.dbi.shift
import dench.bootstrap._

given elabLet: ImplicitAwareInferableElaborator[Let] with
  extension (let: Let) def infer(using Context) = 
    val ds0 :+ in0 = let.items
    val ds = ds0.map(_ match {
      case d: Box => d
      case _ => logicError()
    })
    val in: Term = in0 match {
      case _: Box => logicError()
      case a => a.asInstanceOf[Term]
    }
    val (ms, ctx1) = checkLocalBoxes(ds)
    val (s, t) = in.infer(using ctx1)
    val ms1 = ctx1.finish()
    (syntax.Let(ms, s.shift(0, ms1)), t)
  
  // FEATURE redefine as match checkable Let with implicit insertion
  extension (let: Let) override def check(typ: value.Term)(using Context) = 
    val ds0 :+ in0 = let.items
    val ds = ds0.map(_ match {
      case d: Box => d
      case _ => logicError()
    })
    val in: Term = in0 match {
      case a: Term => a
      case _ => logicError()
    }
    val (ms, ctx1) = checkLocalBoxes(ds)
    val s = in.check(typ)(using ctx1)
    val ms1 = ctx1.finish()
    syntax.Let(ms, s.shift(0, ms1))