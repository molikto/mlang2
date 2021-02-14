package mlang.core

import utils.{ Ref => R, _ }
import org.junit.Test
import org.junit.Assert._
import mlang.core._
import mlang.core.syntax._
import mlang.core.checker._
import scala.language.implicitConversions

implicit def wrap_Explicit_Name(t: Name): Plicit[Name] = Plicit(t, Plicity.Ex)
private implicit def term2Closure(x: Term): Closure = Closure(x)

val program: Program = Seq(
  Box.Decl(Set.Zero),  // nat_decl 
  Box.DefRec(Ref(1), // nat 
    Enum(Seq(
      Telescope.Nil,
      Telescope.Cons(Ref(1) /* nat */, TelescopeClosure(Telescope.Nil))
    ), etyp.Enum(Seq(
      etyp.Kase("zero", Seq.empty),
      etyp.Kase("suc", Seq("prev"))
    )))
  ),

  Box.Decl(Pi(Ref(1), Pi(Ref(2), Ref(3), etyp.Pi("")), etyp.Pi(""))), // add_decl 
  Box.DefRec(Ref(1), // add
    PatternLambda(Seq(
      Branch(Pattern.Construct(0, Seq.empty), Lambda(Ref(1))),
      Branch(Pattern.Construct(1, Seq(Pattern.Generic)), Lambda(Construct(1, App(App(Ref(3), Ref(2)), Ref(1)))))
    ))
  ),

  Box.Def( // two
    Ref(2),
    Construct(1, Construct(1, Construct(0)))
  ),

  Box.Def( // 2 + 2
    Ref(3),
    App(App(Ref(2), Ref(1)), Ref(1))
  ),

  // Box.Def( // 4
  //   Ref(4),
  //   Construct(1, Construct(1, Construct(1, Construct(1, Construct(0)))))
  // ),

  // Box.Def(
  //   Pi(Pi(Ref(5), Set.Zero), Pi(App(Ref(1), Ref(3)), App(Ref(2), Ref(3)))),
  //   Lambda(Lambda(Ref(1)))
  // )
)

class BaseTests {
  @Test def test(): Unit = {
    val res = program.check()
  }
}