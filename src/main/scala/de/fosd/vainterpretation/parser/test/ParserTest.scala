package de.fosd.vainterpretation.parser.test

import de.fosd.typechef.featureexpr.FeatureExprFactory._
import org.junit._
import Assert._
import de.fosd.vainterpretation.parser.WhileParser

class ParserTest {
  
  val parser = new WhileParser()

  
  @Test
  def testParseAssignments() {
      parser.parseFile("programs\\test\\program_assignments.txt")
  }

  @Test
  def testParseExpressions() {
      parser.parseFile("programs\\test\\program_expression.txt")
  }
  
  @Test
  def testParseIf() {
      parser.parseFile("programs\\test\\program_if.txt")
  }
    
  @Test
  def testParseWhile() {
      parser.parseFile("programs\\test\\program_while.txt")
  }
  
  @Test
  def testParseAssertions() {
      parser.parseFile("programs\\test\\program_assertions.txt")
  }
  
  @Test
  def testParseFunctions() {
      parser.parseFile("programs\\test\\program_functions.txt")
  }
  
  @Test
  def testParseClasses() {
    parser.parseFile("programs\\test\\program_classes.txt")
  }
  
  @Test
  def testParseClasses2() {
    parser.parseFile("programs\\test\\program_classes2.txt")
  }
  
  @Test
  def testParseCollections() {
    parser.parseFile("programs\\test\\program_collections.txt")
  }
  
  @Test
  def testParseGPL() {
    parser.parseFile("programs\\test\\program_GPL.txt")
  }
}