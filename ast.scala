package ast

import os.Path
import scala.annotation.tailrec

enum Command:
  case ls
  case lo
  case cd(dir: Path)
  case cat
end Command

case class CommandContext[T](
    currentDir: Path,
    stdout: Iterable[T]
)

object CommandContext:
  def empty[T]: CommandContext[T] =
    CommandContext(os.pwd, Iterable.empty)
end CommandContext

@tailrec
def interpret[T](
    ctx: CommandContext[T],
    chain: List[Command]
): Unit =
  chain match
    case Nil => ()
    case a :: xs =>
      import Command.*
      val CommandContext(currentDir, content) = ctx
      var tail = xs

      val c = a match
        case `ls` => tail = lo :: cat :: xs; ctx
        case `lo` =>
          CommandContext(currentDir, os.list(ctx.currentDir))
        case cd(dir) => CommandContext(dir, content)
        case `cat` =>
          content.foreach(println(_)); ctx

      interpret(c, tail)
