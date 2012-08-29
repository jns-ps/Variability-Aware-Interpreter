package de.puschj.interpreter.test

import de.puschj.interpreter.{VAStore,Assignment,Statement,Mul,Num,IntValue}
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
import de.puschj.interpreter.VAFuncStore
import de.puschj.interpreter.UndefinedValue
import de.puschj.interpreter.VAClassStore
import de.puschj.interpreter.VAStore
import de.puschj.interpreter.VAFuncStore
import com.sun.org.apache.bcel.internal.util.ClassStack
import de.puschj.interpreter.VAInterpreter


class InterpreterTest {
  
  val parser: WhileParser = new WhileParser()
  
  implicit def singleIntValue(value: Int) = One(IntValue(value))
    
  @Test
  def testAssignments() {
    val program: VariableProgram = parser.parseFile("program_assignments.txt")
    val store = program.run()
    store.print("Assignments")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    val fY: FeatureExpr = FeatureExprFactory.createDefinedExternal("Y")
    
    assertTrue("assigning 'a' failed", ConditionalLib.equals(Choice(fA, 1, 2), store.get("a")))
    assertEquals("assigning 'b' failed", One(IntValue(0)), store.get("b"))
    assertTrue("assigning 'c' failed", ConditionalLib.equals(
        Choice(fX, Choice(fY, 2, 1), 0), store.get("c") ))
  }
  
  @Test
  def testExpressions() {
    val program: VariableProgram = parser.parseFile("program_expression.txt")
    val store = program.run()
    store.print("Expressions")
    

    assertEquals("calculating 'x' failed", One(IntValue(2)), store.get("x"))
    assertEquals("calculating 'y' failed", One(IntValue(6)), store.get("y"))
    assertEquals("calculating 'z' failed", One(IntValue(7)), store.get("z"))
  }
  
  @Test
  def testIf() {
    val program: VariableProgram = parser.parseFile("program_if.txt")
    val store = program.run()
    store.print("If")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("'c' incorrectly assigned", ConditionalLib.equals(
        Choice(fA, 1, 0), store.get("c") ))
    assertTrue("unexpected value for 'x'", ConditionalLib.equals(
        Choice(fA.not(), 2, Choice(fB, 1, 3)), store.get("x") ))
  }
  
  @Test
  def testIf2() {
    val program: VariableProgram = parser.parseFile("program_if2.txt")
    val store = program.run()
    store.print("If 2")
    
    assertEquals("unexpected value for 'a'", One(IntValue(10)), store.get("a") )
  }
  
  @Test
  def testWhile() {
    val program: VariableProgram = parser.parseFile("program_while.txt")
    val store = program.run()
    store.print("While")
    
    val fX: FeatureExpr = FeatureExprFactory.createDefinedExternal("X")
    
    assertTrue("unexpected value for 'a'", ConditionalLib.equals(
        Choice(fX, 5, 3), store.get("a") ))
    assertTrue("unexpected value for 'b'", ConditionalLib.equals(
        Choice(fX, 4, 3), store.get("b") ))
  }
  
  @Test
  def testAssertions() {
    val program: VariableProgram = parser.parseFile("program_assertions.txt")
    val store = program.run()
    store.print("Assertions")
    
    // no exception thrown = test successful
    assertTrue(true)
  }
  
//  @Test
//  def testChoiceExplotion() {
//    val program: VariableProgram = parser.parseFile("program_choiceExplotion.txt")
//    val store = program.run().print("Choice Explotion")
//    
//    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
//    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
//    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
//    val fD: FeatureExpr = FeatureExprFactory.createDefinedExternal("D")
//    
//    assertEquals("unexpected value for 'a'", 
//      Choice(fA or fC, 1)), One(UndefinedValue("x not initialized."))),
//      store.get("x")
//    )
//  }
  
  @Test
  def testContextImportance() {
    val program: VariableProgram = parser.parseFile("program_contextImportance.txt")
    val store = program.run()
    store.print("Context Importance")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    
    assertTrue("unexpected value for 'y'", ConditionalLib.equals(
        Choice(fB.not(), One(UndefinedValue("y not initialized.")), Choice(fA.not(), 1, 2)),
        store.get("y")
    ))
  }
  
  @Test
  def testFunctions() {
    val program: VariableProgram = parser.parseFile("program_functions.txt")
    val store = program.run()
    store.print("Functions")
    
    assertEquals("unexpected function return value", One(IntValue(12)), store.get("b"))
  }
  
  @Test
  def testFunctions2() {
    val program: VariableProgram = parser.parseFile("program_functions2.txt")
    val store = program.run()
    store.print("Functions 2")
    
    assertEquals("inner function application failed", One(IntValue(25)), store.get("a"))
  }
  
  @Test
  def testFunctionsRecursion() {
    val program: VariableProgram = parser.parseFile("program_functions_recursion.txt")
    val store = program.run()
    store.print("Functions Recursion")
    
    assertEquals("faculty recursion failed", One(IntValue(720)), store.get("a"))
  }
  
  @Test
  def testFosd12Figure9() {
    val program: VariableProgram = parser.parseFile("program_fosd12_figure9.txt")
    val store = program.run()
    store.print("FOSD 12 figure 9")
    
    val fFOO: FeatureExpr = FeatureExprFactory.createDefinedExternal("FOO")
    
    assertTrue("unexpected value for 'res'", ConditionalLib.equals(
        Choice(fFOO, 6, 2),
        store.get("res")
    ))
  }
  
  @Test
  def testVariableFunctions() {
    val program: VariableProgram = parser.parseFile("program_variablefunctions.txt")
    val store = program.run()
    store.print("Variable Functions")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
    val fS: FeatureExpr = FeatureExprFactory.createDefinedExternal("S")

    assertTrue("unexpected value for 'c'", ConditionalLib.equals(
        Choice(fC, Choice(fS, Choice(fA, 5, 4), One(UndefinedValue("func \"sum\" not declared"))),
                   One(UndefinedValue("c not initialized."))),
        store.get("c")
    ))
  }
  
  @Test
  def testVariableFunctions2() {
    val program: VariableProgram = parser.parseFile("program_variablefunctions2.txt")
    val store = program.run()
    store.print("Variable Functions 2")
    
    val fA: FeatureExpr = FeatureExprFactory.createDefinedExternal("A")
    val fB: FeatureExpr = FeatureExprFactory.createDefinedExternal("B")
    val fC: FeatureExpr = FeatureExprFactory.createDefinedExternal("C")
    
    assertTrue("unexpected value for 'x'", ConditionalLib.equals(
        Choice(fC, Choice(fA, 4, 5), Choice(fB, Choice(fA, 3, 4), One(UndefinedValue("func \"sum\" not declared")))),
        store.get("x")
    ))
  }
  
  @Test
  def testClasses() {
    val program: VariableProgram = parser.parseFile("program_classes.txt")
    val store = program.run()
    store.print("Classes")
  }
  
  @Test
  def testClasses2() {
    val program: VariableProgram = parser.parseFile("program_classes2.txt")
    val store = program.run()
    store.print("Classes 2")
  }
  
//  @Test
//  def testAllFields() {
//    val program: VariableProgram = parser.parseFile("program_classes_inheritance.txt")
//    val classStore = new VAClassStore
//    program.run(new VAStore, new VAFuncStore, classStore)
//    
//    classStore.print()
//    println(VAInterpreter.allFields("B", classStore))
//  }
  
  @Test
  def testGraphProductLine() {
    val program: VariableProgram = parser.parseFile("program_GPL.txt")
    val store = program.run()
    store.print("Graph Product Line")
  }
  
  @Test
  def testCollections() {
    val program: VariableProgram = parser.parseFile("program_collections.txt")
    val store = program.run()
    store.print("Collections")
  }
}