package mlang.core.value

import mlang.core._
import utils._

type BuiltIn = Int => Term

private val notInit: BuiltIn = (a: Int) => logicError()

var top: BuiltIn = notInit
var bottom: BuiltIn = notInit
var refl: BuiltIn = notInit
var sym: BuiltIn = notInit
var trans: BuiltIn = notInit
var ap: BuiltIn = notInit
var iff: BuiltIn = notInit