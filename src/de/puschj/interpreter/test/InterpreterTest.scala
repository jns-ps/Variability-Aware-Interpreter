package de.puschj.interpreter.test

import de.puschj.interpreter.{Environment,Assignment,Statement,Mul,Num}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.One
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.conditional.Conditional
import org.junit._
import Assert._
import de.fosd.typechef.conditional.ConditionalLib
import de.puschj.parser.WhileParser
import de.puschj.interpreter.Program


class InterpreterTest {
  
  var NL: String = "\n"
  var env: Environment = null
  val parser: WhileParser = new WhileParser()
  
  @Before
  def setUp() = {
     env = new Environment()
  }
    
  @Test
  def testAssignments() {
    val program: Program = parser.parseFile("program_assignments.txt")
    program.run(env).print("Assignments")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    val fY: FeatureExpr = FeatureExprFactory.createDefinedExternal("Y")
    
    assertTrue("assigning 'a' failed", ConditionalLib.equals(Choice(fA, One(1), One(2)), env.get("a")))
    assertEquals("assigning 'b' failed", One(0), env.get("b"))
    assertTrue("assigning 'c' failed", ConditionalLib.equals(
        Choice(fX, Choice(fY, One(2), One(1)), One(0)), 
        env.get("c") ))
  }
  
  @Test
  def testExpressions() {
    val program: Program = parser.parseFile("program_expression.txt")
    program.run(env).print("Expressions")
    
    assertEquals("calculating 'x' failed", One(2), env.get("x"))
    assertEquals("calculating 'y' failed", One(6), env.get("y"))
    assertEquals("calculating 'z' failed", One(7), env.get("z"))
  }
  
  @Test
  def testIf() {
    val program: Program = parser.parseFile("program_if.txt")
    program.run(env).print("If")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("'c' incorrectly assigned", ConditionalLib.equals(
        Choice(fA, One(1), One(0)), 
        env.get("c") ))
    assertTrue("unexpected value for 'x'", ConditionalLib.equals(
        Choice(fA.not(), One(2), Choice(fB, One(1), One(3))),
        env.get("x") ))
  }

  @Test
  def testWhile() {
    val program: Program = parser.parseFile("program_while.txt")
    program.run(env).print("While")
    
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    
    assertTrue("unexpected value for 'a'", ConditionalLib.equals(
        Choice(fX, One(5), One(3)), 
        env.get("a") ))
    assertTrue("unexpected value for 'b'", ConditionalLib.equals(
        Choice(fX, One(4), One(3)), 
        env.get("b") ))
  }
  
  @Test
  def testAssertions() {
    val program: Program = parser.parseFile("program_assertions.txt")
    program.run(env).print("Assertions")
    
    // no exception thrown = test successful
    assertTrue(true)
  }
}