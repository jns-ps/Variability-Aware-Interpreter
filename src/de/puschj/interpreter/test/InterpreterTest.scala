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
  
  var env: Environment = null
  val parser: WhileParser = new WhileParser()
  
  @Before
  def setUp() = {
     env = new Environment()
  }
  
  @Test
  def testSimpleProg() {
    val code: String = 
    		"begin\n" +
    		"a = 1;\n" +
    		"b = 1;\n" +
    		"//#ifdef X\n" +
    		"b = 3;\n" +
    		"//#endif\n" +
    		"a = a + b;\n" +
    		"end\n"
    
    val program: Program = parser.runTest(code)
    println(program.run(env).print())
    // TODO: add proper assertions
  }
  
  @Test
  def testInnerIfdef() {
    val code: String = 
    		"begin\n" +
    		"a = 1;\n" +
    		"b = 1;\n" +
    		"//#ifdef X\n" +
    		"b = 3;\n" +
    		"//#ifdef Y\n" +
    		"a = 3;" +
    		"//#endif\n" +
    		"//#endif\n" +
    		"a = a + b;\n" +
    		"end\n"
    
    val program: Program = parser.runTest(code)
    println(program.run(env).print())
    // TODO: add proper assertions
  }
  
}