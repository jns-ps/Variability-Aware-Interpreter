package de.puschj.parser.test

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.io.Source
import de.puschj.parser.WhileParser
import de.puschj.interpreter.Statement
import de.puschj.interpreter.Environment
import de.fosd.typechef.conditional._

object Test {
  def main(args: Array[String]) {
      val parser = new WhileParser()
      val source = scala.io.Source.fromFile("program2.txt")
      val lines = source.mkString
      source.close()
      parser.runTest(lines)
  }
}