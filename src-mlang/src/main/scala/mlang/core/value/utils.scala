package mlang.core.value

object GenericApps:
  def unapply(term: Term): Option[(Generic, Seq[Term])] =
    term match
    case App(l, r) =>
      unapply(l) match
      case Some((g, av)) => Some((g, av :+ r))
      case None => None
    case g: Generic => Some((g, Seq.empty))
    case _ => None


object MetaGenericsApps:
  def unapply(term: Term): Option[(Meta, Seq[Generic])] =
    term match
    case App(l, r) =>
      r.whnf match
      case rr: Generic =>  // should you whnf?
        unapply(l) match
        case Some((g, av)) => Some((g, av :+ rr))
        case None => None
      case _ => None
    case g: Meta => Some((g, Seq.empty))
    case _ => None