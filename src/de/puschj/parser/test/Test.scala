package de.puschj.parser.test

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.io.Source
import de.puschj.parser.StatementParser
import de.puschj.interpreter.Statement
import de.puschj.interpreter.StatementExecutor
import de.puschj.interpreter.Environment

object Test {
  
  
  def main(args: Array[String]) {
      val stmtP = new StatementParser()
      val source = scala.io.Source.fromFile("program.txt")
      val lines = source .mkString
      source.close()

      val env : Environment = new Environment()
      val program =  stmtP.parse(lines).get
      println("====== Program AST =======")
      program.print()
      println("==========================")
      program.run(env)
  }
}