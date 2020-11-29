package mlang.core.value
import mlang.core.syntax


case class Context(tenv: TEnv, env: Env):
  def ext(typ: Term, v: Term): Context = Context(tenv :+ typ, env :+ v)


def check(a: Term, closure: syntax.Closure, typ: Closure1)(using ctx: Context): Unit =
  val v = Var()
  check(closure.get, typ(v))(using ctx.ext(a, v))

def check(term: syntax.Term, typ: Term)(using ctx: Context): Unit =
  (term, typ) match
  case (syntax.Lambda(c), Pi(a, b, _)) => check(a, c, b)
  case _ => conversion(infer(term), typ)

def infer(term: syntax.Term)(using ctx: Context): Term =
  ???
