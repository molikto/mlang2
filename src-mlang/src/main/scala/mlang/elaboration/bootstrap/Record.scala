package mlang.elaboration.bootstrap

import mlang.elaboration._
import mlang.core._
import dench.bootstrap._


given elabRecord: ImplicitAwareInferableElaborator[Record] with
  extension (record: Record) def infer(using Context) =
    val (t, fs, s) = inferTelescope(record.fields)
    return (syntax.Record(t, etyp.Record(fs)), s)