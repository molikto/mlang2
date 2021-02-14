package mlang.elaboration.bootstrap

import mlang.infra._
import org.junit.Test
import org.junit.Assert._
import mlang.elaboration._
import mlang.core
import scala.language.implicitConversions

inline def time[T](a: => T): (T, Long) = {
  val t1 = System.nanoTime()
  val res = a
  val t2 = System.nanoTime()
  (res, t2 - t1)
}

case class Stats(var read: Long = 0, var parse: Long = 0, var check: Long = 0) {

  override def toString = s"read: ${read/100000}ms parse: ${parse/100000}ms, check: ${check/100000}ms"
}

def runAll(folder: os.Path, share: Boolean) = {
  runAllInFolder(folder, folder, if share then Context() else null)
}

def runAllInFolder(root: os.Path, folder: os.Path, ctx: Context | Null): Context | Null = {
  val files = os.list(folder)
  files.foldLeft(ctx) { (ctx, f) =>
    if os.isDir(f) then
      runAllInFolder(root, f, ctx)
    else if f.last.endsWith(".poor") then
      val stats = Stats()
      val fr = f.relativeTo(root)
      var namespace: Seq[String] = fr.segments.dropRight(1).map(a => {
        val segs = a.split("\\.").asInstanceOf[Array[String]]
        if segs.size > 1 && segs(0).forall(_.isDigit) then segs(1)
        else segs(0)
      })
      if (namespace == Seq("prelude")) then namespace = Seq.empty
      trace.enter("checking file " + f)
      println(fr)
      val (bs, t1) = time { os.read.bytes(f) }
      stats.read += t1
      val (tree, t2) = time{ dench.bootstrap.parse(bs) }
      stats.parse += t2
      trace.log(s"parse result: $tree")
      val (res, t3) = time {
        val ctx0 = if ctx == null then Context() else ctx
        val ctxr = check(namespace, tree)(using ctx0)
        if (ctx == null) then null else ctxr
      }
      stats.check += t3
      trace.exit()
      println(stats.toString)
      res
    else
      ctx
  }
}

class BasicTests {
  @Test def test(): Unit = {
    runAll(os.pwd/"src-mlang"/"test", false)
    runAll(os.pwd/"src-mlang"/"bootstrap", true)
  }
}