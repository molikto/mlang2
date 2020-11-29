package mlang.core.common

sealed trait Pattern:
  def size: Int

object Pattern:
  case object Generic extends Pattern:
    def size = 1 

  case class Make(ps: Seq[Pattern]) extends Pattern:
    val size = ps.map(p => p.size).sum

  case class Construct(name: Int, ps: Seq[Pattern]) extends Pattern:
    val size = ps.map(p => p.size).sum
