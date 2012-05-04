package de.puschj.parser.test

import de.puschj.parser.WhileParser
import org.junit.Test
import org.junit.Before

class ParserTest {
  
  val parser = new WhileParser()

  
  @Test
  def testAssignments() {
      parser.parseFile("program_assignments.txt")
  }

  @Test
  def testExpressions() {
      parser.parseFile("program_expression.txt")
  }
  
  @Test
  def testIf() {
      parser.parseFile("program_if.txt")
  }
    
  @Test
  def testWhile() {
      parser.parseFile("program_while.txt")
  }
  
  @Test
  def testAssertions() {
      parser.parseFile("program_assertions.txt")
  }
}