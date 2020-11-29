package mlang.core.common

opaque type Name = String
object Name:
  def apply(str: String): Name = str

enum EType:
  case Record(names: Seq[Name])
  case Sum(names: Seq[Name])
  case Pi()

