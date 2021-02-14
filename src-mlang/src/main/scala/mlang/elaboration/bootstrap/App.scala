package mlang.elaboration.bootstrap

import utils._
import mlang.elaboration._
import mlang.core.translate.eval
import mlang.core.value.whnf
import mlang.core._
import dench.bootstrap._

class CannotApplyBecuaseNotPiException(a: String) extends NotHandledException(a)

given elabApp: ImplicitAwareInferableElaborator[App] with
  extension (thiz: App) override def check(typ: value.Term)(using ctx: Context) = 
    val App(head: Term, args) = thiz
    def fallback() = super.check(thiz)(typ)
    head match
    case r: Ref =>
      ctx(r, 0) match
      case SpecialRef.Make =>
        typ.whnf match
        case value.Unwrap(value.Record(fs, e)) =>
          syntax.Make(checkByTelescope(fs, e.fields, args))
        case _ => notHandled()
      case _ =>
        r match
        case ref: Name =>
          typ.whnf match
          case value.Unwrap(value.Enum(kases, e)) =>
            e(ref) match
            case (index, k) =>
              syntax.Construct(index, checkByTelescope(kases(index), k.fields, args))
            case _ => fallback()
          case _ => fallback()
        case _ => fallback()
    case _ => fallback()

  extension (thiz: App) def infer(using Context) =
    val App(head: Term, args) = thiz
    inferApp(head, args)
  
private def inferApp(head: Term, args: Seq[PlicitTerm])(using Context): InferResult = 
  var canTryProj = true
  try {
    args.foldLeft(head.infer) { (h, a) =>
      val ret = rec(h, a)
      canTryProj = false
      ret
    }
  } catch {
    case e: CannotApplyBecuaseNotPiException if canTryProj =>
      App(Proj(head, "apply"), args).infer
    case e => throw e
  }

private def rec(head: InferResult, arg: PlicitTerm)(using Context): InferResult =
  val (s, ft) = insertImplicitApps(head, arg.plicity)
  ft.whnf match
  case value.Unwrap(value.Pi(a, b, e)) =>
    if arg.plicity == e.name.plicity then
      val res = arg.unwrap.check(a)
      (syntax.App(s, res), b(res.eval))
    else notHandled(head)
  case a =>
    println(a)
    throw CannotApplyBecuaseNotPiException(arg.toString)
