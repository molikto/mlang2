package mlang.elaboration.bootstrap

import utils._
import mlang.core.{ Set => S, Prop => P, _ }
import mlang.core.value.whnf
import mlang.elaboration._
import dench.bootstrap._
  

case object InferErrorRefHack extends Exception()

given elabRef: ImplicitAwareInferableElaborator[Ref | LiftRef] with
  extension (ref: Ref | LiftRef) def infer(using ctx: Context) =
    val LiftRef(name, up) = ref match {
      case l@LiftRef(n, u) => l
      case r: Ref => LiftRef(r, 0)
    }
    ctx(name, up) match
    case SpecialRef.Error => throw InferErrorRefHack
    case _: SpecialRef => notHandled()
    case (s, v) => (s, v.typ)
    case _ => 
      throw NotResolvedException(name)

  extension (ref: Ref | LiftRef) override def check(typ: value.Term)(using Context) = 
    try {
      super.check(ref)(typ)
    } catch {
      case InferErrorRefHack => syntax.Error
      case exp: NotResolvedException => // TODO refactor without exception
        ref match
        case ref: Name => // only case is not lifted and not seq
          typ.whnf match
          case value.Unwrap(value.Enum(kases, e)) =>
            e(ref) match
            case (index, k) if k.fields.size == 0 =>
              syntax.Construct(index, Seq.empty)
            case _ => throw exp
          case _ => throw exp
        case _ => throw exp
    }