package mlang.elaboration

import mlang.core._
import utils._
import mlang.core.value.whnf
import mlang.core.translate.reify

def notHandled(a: Any = "") =
  throw new NotHandledException(a.toString)
class NotHandledException(msg: String) extends Exception(msg)

class ConversionFailedException(str: String) extends Exception(str)

type InferResult = (syntax.Term, value.Term) // term and type
type InferAsTypResult = (syntax.Term, Sort)

trait Elaborator[T]:
  extension (thiz: T) def check(typ: value.Term)(using Context): syntax.Term
  extension (thiz: T) def infer(using Context): InferResult

  extension (thiz: T) def inferAsSort(using Context): Sort =
    thiz.infer._1 match
    case s: Sort => s
    case _ => notHandled()

  extension (thiz: T) def inferAsTyp(using Context): InferAsTypResult =
    val res = thiz.infer
    val (s, t) = res
    t.whnf match
    case t_ : Sort => (s, t_)
    case a => notHandled(thiz)

trait InferableElaborator[T] extends Elaborator[T]:
  extension (thiz: T) def check(typ: value.Term)(using ctx: Context): syntax.Term = 
    val (s, t) = thiz.infer
    if ctx.subtyp(t, typ) then s else
      throw ConversionFailedException(s"1. ${t.whnf.reify}\n2. ${typ.whnf.reify}")

trait CheckOnlyElaborator[T] extends Elaborator[T]:
  extension (thiz: T) def infer(using Context) = logicError()