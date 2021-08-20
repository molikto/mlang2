package mlang.elaboration.bootstrap

import mlang.elaboration._
import mlang.core._
import dench.bootstrap._

given elabEnum: ImplicitAwareInferableElaborator[Enum] with
  extension (en: Enum) def infer(using Context) = 
    val ms = en.kases.map(k => {
      val (t, fs, s) = inferTelescope(k.fields)
      if k.fields.size == 1 && k.name == "suc" then
        (t.asInstanceOf[syntax.Telescope.Cons].head, etyp.Kase(k.name, false), s)
      else 
        (syntax.Record(t, etyp.Record(fs)), etyp.Kase(k.name, true), s)
    })
    val e = etyp.Enum(ms.map(_._2))
    val syn = syntax.Enum(ms.map(_._1), e)
    (syn, Sort.ind(ms.map(_._3)))