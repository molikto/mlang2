package mlang.elaboration.bootstrap

import mlang.elaboration._
import mlang.core._
import dench.bootstrap._

given elabEnum: ImplicitAwareInferableElaborator[Enum] with
  extension (en: Enum) def infer(using Context) = 
    val ms = en.kases.map(k => {
      val (t, fs, s) = inferTelescope(k.fields)
      (t, etyp.Kase(k.name, fs), s)
    })
    val e = etyp.Enum(ms.map(_._2))
    val syn = syntax.Enum(ms.map(_._1), e)
    (syn, Sort.ind(ms.map(_._3)))