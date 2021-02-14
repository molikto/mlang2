package mlang.infra

import utils.DEV
import scala.collection.mutable

val TRACE = DEV && true

type Message = String
class Frame(
  var start: Message | Null = null,
  val inner: mutable.Stack[Frame | Message] = new mutable.Stack(),
  var end: Message | Null = null)

private val Spaces = "                                                                                                                                 "

class Tracer:
  private val root: Frame = Frame()
  var disabled = false
  private[infra] val stack: mutable.Stack[Frame] = new mutable.Stack()
  stack.push(root)

  private val file = os.pwd/"res.txt"
  if os.exists(file) then os.remove(file)

  private[infra] def print(arg: => Message) = 
    os.write.append(file, Spaces.take(stack.size * 2 - 2) + arg + "\n")

  inline def disable(): Unit =
    if TRACE then
      trace.disabled = true

  inline def enable(): Unit =
    if TRACE then
      trace.disabled = false

  inline def log(arg: => Message): Unit =
    if TRACE then
      if !disabled then
        print(arg)
        stack.last.inner.push(arg)

  inline def enter(arg: => Message): Unit =
    if TRACE then
      if !disabled then
        print("[+] " + arg + " {")
        val f = Frame()
        f.start = arg
        stack.last.inner.push(f)
        stack.push(f)

  inline def exit(arg: => Message | Null = null): Unit =
    if TRACE then
      if !disabled then
        stack.last.end = arg
        if arg != null then print("[-] " + arg)
        stack.pop()
        print("}")


val trace = Tracer()