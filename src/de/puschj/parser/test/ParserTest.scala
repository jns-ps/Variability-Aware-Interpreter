package de.puschj.parser.test

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.lexical.StdLexical
import scala.io.Source
import de.puschj.parser.WhileParser
import de.puschj.interpreter.Statement
import de.puschj.interpreter.Environment
import de.fosd.typechef.conditional._
import org.junit.Test
import org.junit.Before

class ParserTest {
  
  val parser = new WhileParser()

  @Test
  def whileTest() {
      val source = scala.io.Source.fromFile("program_while.txt")
      val input = source.mkString
      source.close()
      parser.parse(input)
  }
  
  @Test
  def ifTest() {
      val source = scala.io.Source.fromFile("program_if.txt")
      val input = source.mkString
      source.close()
      parser.parse(input)
  }
    
  @Test
  def expressionTest() {
      val source = scala.io.Source.fromFile("program_expression.txt")
      val input = source.mkString
      source.close()
      parser.parse(input)
  }
  
  @Test
  def assignmentsTest() {
      val source = scala.io.Source.fromFile("program_assignments.txt")
      val input = source.mkString
      source.close()
      parser.parse(input)
  }
}