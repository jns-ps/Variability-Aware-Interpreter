package de.puschj.interpreter.test

import de.puschj.interpreter.{Store,Assignment,Statement,Mul,Num}
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
  var env: Store = null
  val parser: WhileParser = new WhileParser()
  
  @Before
  def setUp() = {
     env = new Store()
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
  
  @Test
  def testChoiceExplotion() {
    val program: Program = parser.parseFile("program_choiceExplotion.txt")
    program.run(env).print("Choice Explotion")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
    val fD: FeatureExpr = FeatureExprFactory.createDefinedExternal("D")
    
//    assertEquals("unexpected value for 'a'", 
//      Choice(fA.and(fC), One(1), One)
//    
//    )
    
  }
  
  @Test
  def testContextImportance() {
    val program: Program = parser.parseFile("program_contextImportance.txt")
    program.run(env).print("Context Importance")
    
    
  }
  
  @Test
  def testMapping() {
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    var x: Conditional[Int] = One(1).mapfr(fA, (fexpr, a) => Choice(fA.and(fexpr), One(a+10), One(a)))
//    var x: Conditional[Int] = Choice(fB, One(1), One(2)).mapfr(fA, (fexpr, a) => Choice(fA.and(fexpr), One(a+10), One(a)))
    
    println(x.simplify)
  }
}