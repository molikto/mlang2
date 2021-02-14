package mlang.elaboration.bootstrap

import dench.bootstrap._
import mlang.elaboration._

def check(namespace: Seq[String], source: SourceFile)(using ctx: Context): Context =
  checkGlobals(source.boxes)(using ctx.withNamespace(namespace))