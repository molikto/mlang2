package mlang.core.common

case object Prop
case class Set(l: Int)
type Sort = Prop.type | Set
