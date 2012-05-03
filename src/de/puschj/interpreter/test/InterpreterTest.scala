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
    val source = scala.io.Source.fromFile("program_assignments.txt")
    val input = source.mkString
    source.close()
    val program: Program = parser.parse(input)
    println(program.run(env).print())
    // TODO: add proper assertions
  }
  
  @Test
  def testIf() {
    val source = scala.io.Source.fromFile("program_if.txt")
    val input = source.mkString
    source.close()
    val program: Program = parser.parse(input)
    println(program.run(env).print())
    // TODO: add proper assertions
  }

  @Test
  def testWhile() {
    val source = scala.io.Source.fromFile("program_while.txt")
    val input = source.mkString
    source.close()
    val program: Program = parser.parse(input)
    println(program.run(env).print())
    // TODO: add proper assertions

  }
  
}