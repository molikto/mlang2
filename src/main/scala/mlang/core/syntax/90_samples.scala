package mlang.core.syntax
import mlang.core.common.{ Name, EType }

val nat = Sum(
  Seq(
    Telescope(),
    Telescope(Ref(0))
  ),
  EType.Sum(Seq(Name("zero"), Name("suc")))
)