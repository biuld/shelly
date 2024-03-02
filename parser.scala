package parser

import os.Path
import os.RelPath
import parsley.Parsley
import parsley.character
import parsley.character.string
import parsley.character.whitespace
import parsley.combinator.option
import parsley.combinator.sepBy
import parsley.errors.combinator.ErrorMethods

import java.io.File

import Parsley.{many, atomic, some}

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

  def atom = option(symbol("..") | symbol(".") | str)

  def subPath =
    for xs <- sepBy(atom, symbol("/"))
    yield xs.map(_.getOrElse("")).mkString("/")

  def absPath = symbol("/") concat subPath

  def path: Parsley[Path] =
    for
      p <- subPath | absPath
      p1 = if p.contains(".") then File(p).getCanonicalPath() else p
    yield os.Path(p1)

end Dir

object Command:

  import ast.Command.*
  import ast.FilterOption.*

  def cd = for
    _ <- lexeme(symbol("cd"))
    p <- lexeme(Dir.path)
  yield CD(p)

  def cat = lexeme(symbol("cat")) #> CAT

  def filterOption =
    val suffix = for
      _ <- lexeme(symbol("suffix"))
      _ <- lexeme(symbol("."))
      s <- lexeme(str)
    yield SUFFIX("." + s)

    val regx = for
      _ <- lexeme(symbol("regx"))
      s <- lexeme(str)
    yield REGX(s)

    symbol("isFile") #> ISFILE
      | symbol("isDir") #> ISDIR
      | regx
      | suffix

  def filter = for
    _ <- lexeme(symbol("filter"))
    ops <- filterOption
  yield FILTER(ops)

  def rm = lexeme(symbol("rm")) #> RM

  def decompress = lexeme(symbol("decompress")) #> DECOMPRESS

  def flatten = lexeme(symbol("flatten")) #> FLATTEN

  def atom = cd | cat | filter | rm | decompress | flatten

  def commands = for
    a <- atom
    t <- option(some(lexeme(symbol("|")) ~> atom))
  yield t match
    case None     => a :: Nil
    case Some(t1) => a :: t1

end Command
