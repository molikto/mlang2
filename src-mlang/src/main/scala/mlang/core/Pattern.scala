package mlang.core

sealed trait Pattern derives CanEqual:
  def size: Int

object Pattern:
  case object Generic extends Pattern:
    def size = 1 

  case class Make(fields: Seq[Pattern]) extends Pattern:
    val size = fields.map(p => p.size).sum

  case class Construct(kase: Int, p: Pattern) extends Pattern:
    val size = p.size
