package main

import ast.Command.*
import ast.FilterOption.*
import os.Path

@main
def main: Unit =
  // val p = os.Path("/Users/biu/Downloads/未命名文件夹")
  // val comms = CD(p) :: FILTER(SUFFIX(".zip")) :: DECOMPRESS :: FLATTEN :: Nil

  val comms = parser.Command.commands
    .parse(
      "cd /Users/biu/Downloads/未命名文件夹 | filter suffix .zip | decompress | flatten"
    )

  println(comms)

  ast.interpret(comms.get)
