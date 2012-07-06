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
import de.puschj.interpreter.VariableProgram
import de.puschj.interpreter.FuncStore
import de.puschj.interpreter.UndefinedValue
import de.puschj.interpreter.UndefinedValue


class InterpreterTest {
  
  var store: Store = null
  var funcStore: FuncStore = null
  val parser: WhileParser = new WhileParser()
  
  @Before
  def setUp() = {
     store = new Store()
     funcStore = new FuncStore()
  }
    
  @Test
  def testAssignments() {
    val program: VariableProgram = parser.parseFile("program_assignments.txt")
    program.run(store, funcStore).print("Assignments")
    
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
    val program: VariableProgram = parser.parseFile("program_expression.txt")
    program.run(store, funcStore).print("Expressions")
    

    assertEquals("calculating 'x' failed", One(IntValue(2)), store.get("x"))
    assertEquals("calculating 'y' failed", One(IntValue(6)), store.get("y"))
    assertEquals("calculating 'z' failed", One(IntValue(7)), store.get("z"))
  }
  
  @Test
  def testIf() {
    val program: VariableProgram = parser.parseFile("program_if.txt")
    program.run(store, funcStore).print("If")
    
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
  def testIf2() {
    val program: VariableProgram = parser.parseFile("program_if2.txt")
    program.run(store, funcStore).print("If 2")
    program.printAST
    
    assertEquals("unexpected value for 'a'", 
        One(IntValue(10)),
        store.get("a") )
  }
  
  @Test
  def testWhile() {
    val program: VariableProgram = parser.parseFile("program_while.txt")
    program.run(store, funcStore).print("While")
    
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
    val program: VariableProgram = parser.parseFile("program_assertions.txt")
    program.run(store, funcStore).print("Assertions")
    
    // no exception thrown = test successful
    assertTrue(true)
  }
  
//  @Test
//  def testChoiceExplotion() {
//    val program: VariableProgram = parser.parseFile("program_choiceExplotion.txt")
//    program.run(store, funcStore).print("Choice Explotion")
//    
//    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
//    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
//    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
//    val fD: FeatureExpr = FeatureExprFactory.createDefinedExternal("D")
//    
//    assertEquals("unexpected value for 'a'", 
//      Choice(fA or fC, One(IntValue(1)), One(UndefinedValue("x not initialized."))),
//      store.get("x")
//    )
//  }
  
  @Test
  def testContextImportance() {
    val program: VariableProgram = parser.parseFile("program_contextImportance.txt")
    program.run(store, funcStore).print("Context Importance")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("unexpected value for 'y'", ConditionalLib.equals(
        Choice(fB.not(), One(UndefinedValue("y not initialized.")), Choice(fA.not(), One(IntValue(1)), One(IntValue(2)))),
        store.get("y")
    ))
  }
  
  @Test
  def testFunctions() {
    val program: VariableProgram = parser.parseFile("program_functions.txt")
    program.run(store, funcStore).print("Functions")
    
    // TODO: add proper assertion
  }
  
  @Test
  def testFunctions2() {
    val program: VariableProgram = parser.parseFile("program_functions2.txt")
    program.run(store, funcStore).print("Functions 2")
    
    // TODO: add proper assertion
  }
  
  @Test
  def testFunctionsRecursion() {
    val program: VariableProgram = parser.parseFile("program_functions_recursion.txt")
    program.run(store, funcStore).print("Functions Recursion")
    
    assertEquals("faculty recursion not working", One(IntValue(720)), store.get("a"))
  }
  
  @Test
  def testFosd12Figure9() {
    val program: VariableProgram = parser.parseFile("program_fosd12_figure9.txt")
    program.run(store, funcStore).print("FOSD 12 figure 9")
    
    // TODO: add proper assertion
  }
}