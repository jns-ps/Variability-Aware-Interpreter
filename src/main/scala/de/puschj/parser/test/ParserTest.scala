package de.puschj.parser.test

import de.puschj.parser.WhileParser
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import org.junit._
import Assert._

class ParserTest {
  
  val parser = new WhileParser()

  
  @Test
  def testParseAssignments() {
      parser.parseFile("program_assignments.txt")
      assertTrue(true)
  }

  @Test
  def testParseExpressions() {
      parser.parseFile("program_expression.txt")
  }
  
  @Test
  def testParseIf() {
      parser.parseFile("program_if.txt")
  }
    
  @Test
  def testParseWhile() {
      parser.parseFile("program_while.txt")
  }
  
  @Test
  def testParseAssertions() {
      parser.parseFile("program_assertions.txt")
  }
  
  @Test
  def testParseFunctions() {
      parser.parseFile("program_functions.txt")
  }
  
  @Test
  def testParseBDDs() {
      setDefault(bdd)
    
      val fA = createDefinedExternal("A")
      val fB = createDefinedExternal("B")
      
      val fe1 = (fA.not and fB) or fA
      println(fe1)
      
      val fe2 = (fA and fB).not or fA
      println(fe2)
      
//      parser.parseFile("program_functions.txt")
  }
}