package mlang.elaboration.bootstrap

import mlang.infra._
import utils._
import mlang.elaboration._
import mlang.core._
import mlang.core.value.whnf
import mlang.core.translate.eval
import mlang.core.value.apply
import dench.bootstrap._


given elabProj: ImplicitAwareInferableElaborator[Proj] with
  extension (proj: Proj) def infer(using ctx: Context) =
    val Proj(head: Term, ref) = proj
    val (hs, ht) = head.infer
    val hse = hs.eval
    def fallback() =
      val (name, lvl) = ref match
      case r: Ref => (r, 0)
      case LiftRef(n, l) => (n, l)
      // FIXME also check that all added meta variables are solved
      val t: InferResult = ctx.applyExtension(name, lvl, (rs0, inv) => {
        //trace.disable()
        val snap = ctx.snapshot()
        try {
          val (rs, rt) = insertImplicitApps((rs0, inv.typ), Plicity.Ex)
          rt.whnf match
          case value.Unwrap(value.Pi(a, c, _)) =>
            val t = head.check(a)
            (syntax.App(rs, t), c(t.eval))
          case _ => notHandled()
        } catch {
          case e: ConversionFailedException =>
            ctx.rollback(snap)
            null
          case e => throw e
        } finally {
          trace.enable()
        }
      })
      t
    ref match
    case ref: Name => 
      ht.whnf match
      case value.Unwrap(value.Record(fs, e)) =>
        e(ref) match
        case (index, field) =>
          (syntax.Proj(hs, index), fs.apply(hse, index))
        case null => fallback()
      case _ => fallback()
    case _ => fallback()