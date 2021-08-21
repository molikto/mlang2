package mlang.core.value


case class EvaluationContext(
  val boxes: Map[Box, Term]
): 
  def get(a: Box): Option[Term] = boxes.get(a)