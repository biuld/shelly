package main

@main
def main =
  import ast.*
  import util.*
  import parser.Command.commands

  val exec = interpret.curried(CommandContext.empty)

  for cmms <- commands.parse("cd /Users/biu | ls")
  yield exec(cmms)
