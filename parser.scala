package parser

import parsley.character.{whitespace, string}
import parsley.Parsley, Parsley.{many, atomic, some}
import parsley.errors.combinator.ErrorMethods //for hide
import parsley.combinator.{sepBy, option}
import parsley.character
import os.{Path, RelPath}

val skipWhitespace = many(whitespace.void).void.hide
def symbol(str: String) = atomic(string(str))
def lexeme[A](p: Parsley[A]) = p <~ skipWhitespace
def token[A](p: Parsley[A]) = lexeme(atomic(p))

val str = token(some(character.letterOrDigit).map(_.mkString))

extension (a: Parsley[String])
  infix def concat(b: Parsley[String]) =
    for
      s1 <- a
      s2 <- b
    yield s1 + s2

object Dir:

  val atom = option(symbol("..") | symbol(".") | str)

  def subPath =
    for xs <- sepBy(atom, symbol("/"))
    yield xs.map(_.getOrElse("")).mkString("/")

  def absPath = symbol("/") concat subPath

  // TODO produces a Path from RelPath
  def path: Parsley[Path | RelPath] = (subPath | absPath).map(p =>
    if p.contains(".") then os.RelPath(p) else os.Path(p)
  )

end Dir

object Command:

  import ast.Command as mc

  def cd = for
    _ <- lexeme(symbol("cd"))
    p <- lexeme(Dir.absPath)
  yield mc.cd(os.Path(p))

  def lo = lexeme(symbol("lo")) #> mc.lo

  def cat = lexeme(symbol("cat")) #> mc.cat

  def ls = lexeme(symbol("ls")) #> mc.ls

  def atom = cd | lo | cat | ls

  def commands = for
    a <- atom
    t <- option(some(lexeme(symbol("|")) ~> atom))
  yield t match
    case None     => a :: Nil
    case Some(t1) => a :: t1

end Command
