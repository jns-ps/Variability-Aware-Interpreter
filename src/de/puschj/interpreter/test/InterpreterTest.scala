package de.puschj.interpreter.test

import de.puschj.interpreter.{Store,Assignment,Statement,Mul,Num,IntValue}
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
  var store: Store = null
  val parser: WhileParser = new WhileParser()
  
  @Before
  def setUp() = {
     store = new Store()
  }
    
  @Test
  def testAssignments() {
    val program: Program = parser.parseFile("program_assignments.txt")
    program.run(store).print("Assignments")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    val fY: FeatureExpr = FeatureExprFactory.createDefinedExternal("Y")
    
    assertTrue("assigning 'a' failed", ConditionalLib.equals(Choice(fA, One(IntValue(1)), One(IntValue(2))), store.get("a")))
    println(store.get("a"))
    assertEquals("assigning 'b' failed", One(IntValue(0)), store.get("b"))
    assertTrue("assigning 'c' failed", ConditionalLib.equals(
        Choice(fX, Choice(fY, One(IntValue(2)), One(IntValue(1))), One(IntValue(0))), 
        store.get("c") ))
  }
  
  @Test
  def testExpressions() {
    val program: Program = parser.parseFile("program_expression.txt")
    program.run(store).print("Expressions")
    
    assertEquals("calculating 'x' failed", One(IntValue(2)), store.get("x"))
    assertEquals("calculating 'y' failed", One(IntValue(6)), store.get("y"))
    assertEquals("calculating 'z' failed", One(IntValue(7)), store.get("z"))
  }
  
  @Test
  def testIf() {
    val program: Program = parser.parseFile("program_if.txt")
    program.run(store).print("If")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("'c' incorrectly assigned", ConditionalLib.equals(
        Choice(fA, One(IntValue(1)), One(IntValue(0))), 
        store.get("c") ))
    assertTrue("unexpected value for 'x'", ConditionalLib.equals(
        Choice(fA.not(), One(IntValue(2)), Choice(fB, One(IntValue(1)), One(IntValue(3)))),
        store.get("x") ))
  }

  @Test
  def testWhile() {
    val program: Program = parser.parseFile("program_while.txt")
    program.run(store).print("While")
    
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    
    assertTrue("unexpected value for 'a'", ConditionalLib.equals(
        Choice(fX, One(IntValue(5)), One(IntValue(3))), 
        store.get("a") ))
    assertTrue("unexpected value for 'b'", ConditionalLib.equals(
        Choice(fX, One(IntValue(4)), One(IntValue(3))), 
        store.get("b") ))
  }
  
  @Test
  def testAssertions() {
    val program: Program = parser.parseFile("program_assertions.txt")
    program.run(store).print("Assertions")
    
    // no exception thrown = test successful
    assertTrue(true)
  }
  
  @Test
  def testChoiceExplotion() {
    val program: Program = parser.parseFile("program_choiceExplotion.txt")
    program.run(store).print("Choice Explotion")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
    val fD: FeatureExpr = FeatureExprFactory.createDefinedExternal("D")
    
    assertEquals("unexpected value for 'a'", 
      Choice(fA or fC, One(IntValue(1)), One(IntValue(0))),
      store.get("a")
    )
  }
  
  @Test
  def testContextImportance() {
    val program: Program = parser.parseFile("program_contextImportance.txt")
    program.run(store).print("Context Importance")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("unexpected value for 'y'", ConditionalLib.equals(
        Choice(fB.not(), One(IntValue(0)), Choice(fA.not(), One(IntValue(1)), One(IntValue(2)))),
        store.get("y")
    ))
  }
  
//  @Test
//  def testMapping() {
//    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
//    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
//    
//    var x: Conditional[Int] = One(IntValue(1).mapfr(fA, (fexpr, a) => Choice(fA.and(fexpr), One(IntValue(a+10), One(IntValue(a)))
////    var x: Conditional[Int] = Choice(fB, One(IntValue(1), One(IntValue(2)).mapfr(fA, (fexpr, a) => Choice(fA.and(fexpr), One(IntValue(a+10), One(IntValue(a)))
//    
//    println(x.simplify)
//  }
}