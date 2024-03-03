package ast

import os.Path
import parser.Dir.path

import scala.annotation.tailrec
import java.io.File

enum FilterOption:
  case ISFILE
  case ISDIR
  case REGX(pattern: String)
  case SUFFIX(s: String)

enum Command:
  case CD(dir: Path)
  case FILTER(p: FilterOption)
  case FLATTEN // moves all nested files to the directory in the context
  case CAT
  case RM
  case DECOMPRESS
end Command

case class CommandContext[T <: Path](
    currentDir: Path,
    stdout: Iterable[T]
)

object CommandContext:
  def empty[T <: Path]: CommandContext[T] =
    CommandContext(os.pwd, Iterable.empty)
end CommandContext

@tailrec
def interpret[T <: Path](
    chain: List[Command],
    ctx: CommandContext[T] = CommandContext.empty
): Unit =
  chain match
    case Nil => ()
    case a :: xs =>
      import Command.*
      val CommandContext(currentDir, content) = ctx
      var tail = xs

      val c = a match
        case CD(dir) => CommandContext(dir, os.list(dir))
        case FILTER(p) =>
          p match
            case FilterOption.ISFILE =>
              CommandContext(currentDir, content.filter(os.isFile(_)))
            case FilterOption.ISDIR =>
              CommandContext(currentDir, content.filter(os.isDir(_)))
            case FilterOption.REGX(pattern) =>
              CommandContext(
                currentDir,
                content.filter(f => pattern.r.matches(f.toString))
              )
            case FilterOption.SUFFIX(s) =>
              CommandContext(currentDir, content.filter(_.toString.endsWith(s)))
        case `CAT` =>
          content.foreach(println(_))
          CommandContext(currentDir, Iterable.empty)
        case `RM` =>
          content.foreach(os.remove(_));
          CommandContext(currentDir, Iterable.empty)
        case `FLATTEN` =>
          for
            f <- os.walk(currentDir)
            if currentDir.relativeTo(f) != os.up
          do
            val rel = s"${f.relativeTo(currentDir)}"
            val filename = rel.replace(File.separator, "_")

            if os.exists(currentDir / filename) then
              println(s"$filename exists")
            else
              os.move(f, currentDir / filename)
              println(s"moved ${rel} to ${filename}")

          CommandContext(currentDir, Iterable.empty)
        case `DECOMPRESS` =>
          content.foreach(adapter.decompress(_));
          CommandContext(currentDir, Iterable.empty)

      interpret(tail, c)
