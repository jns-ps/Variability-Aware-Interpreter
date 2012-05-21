package de.puschj.interpreter.test

import org.scalacheck._
import scala.collection.mutable.Set
import Gen._
import Arbitrary.arbitrary
import de.puschj.interpreter._
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Opt
import de.puschj.interpreter.Environment
import de.puschj.interpreter.Environment

object InterpreterAutoCheck extends Properties("Interpreter") {
  
  // FeatureExpressions
  val featureNames = List("A","B","C","D","E","F")
  val genAtomicFeatureExpression =
        oneOf(True :: False :: featureNames.map(createDefinedExternal(_)))
            
  def genCompoundFeatureExpr(size: Int) = oneOf(
    for {
        a <- genFeatureExprSized(size)
        b <- genFeatureExprSized(size)
    } yield a and b,
    for {
        a <- genFeatureExprSized(size)
        b <- genFeatureExprSized(size)
    } yield a or b,
    for {
        a <- genFeatureExprSized(size)
    } yield a.not
  )
  
  def genFeatureExprSized(size: Int): Gen[FeatureExpr] = {
    if (size <= 0) genAtomicFeatureExpression
    else Gen.frequency( (1, genAtomicFeatureExpression), (3, genCompoundFeatureExpr(size/2)) )
  }
  
  def genFeatureExpr() = Gen.sized(size => genFeatureExprSized(size))
  
  implicit def arbFeatureExpression: Arbitrary[FeatureExpr] = Arbitrary {
    genFeatureExpr
  }
  

  // === Expressions === 
  val genVarName = oneOf("a", "b", "c", "d", "e")
        
  def genAtomicExpression() = {
    val genId = for {
      name <- genVarName
    } yield Id(name)
  
    val genNum = for {
      n <- Gen.choose(1, 100)
    } yield Num(n)
    
    oneOf(genId, genNum)
  } 
  
  def genCompoundExpression(size: Int) = {
	  val genAdd = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Add(left,right)
	  
	  val genSub = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Sub(left,right)
	    
	  val genMul = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Mul(left,right)
	    
//	  val genDiv = for {
//	    left <- lzy(genExpressionSized(size))
//	    right <- genExpressionSized(size)
//	  } yield Div(left,right)
    
	  Gen.oneOf(genAdd, genSub, genMul) //genDiv
  }
  
  def genExpressionSized(size: Int): Gen[Expression] = {
    if (size <= 0) genAtomicExpression
    else Gen.frequency( (1, genAtomicExpression), (1, genCompoundExpression(size / 2)))
  }
  
  def genExpression = Gen.sized(size => genExpressionSized(size))
  
  // === Conditions ===
  val genEQ = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield Equal(left, right)
  
  val genGT = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield GreaterThan(left, right)
  
 val genGOE = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield GreaterOrEqualThan(left, right)
  
 val genLT = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield LessThan(left, right)
  
  val genLOE = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield LessOrEqualThan(left, right)
  
  def genCondition = oneOf(genEQ, genGT, genGOE, genLT, genLOE)
  
  // === Statements ===
  val genAssignment = for {
    name <- genVarName
    value <- genExpression
  } yield Assignment(name, value)
  
  val genWhile = for {
    cond <- genCondition
    stmt <- genStatement
  } yield While(cond, stmt)
  
  val genStatement: Gen[Statement] = oneOf(genAssignment, genWhile)
  
  def genOptStatement: Gen[Opt[Statement]] = for {
    feat <- genFeatureExpr
    stmt <- genStatement
  } yield Opt(feat, stmt)
  
  def genProgram: Gen[Program] = Gen.sized( size => for {
    stmts <- listOfN(size, genOptStatement)
  } yield Program(stmts) )
  
  implicit def arbProgram: Arbitrary[Program] = Arbitrary {
    genProgram
  }
    
  property("test") = 
    Prop.forAll( (p: Program) => {
    val env = new Environment()
    p.print()
//    try {
//      p.run(env).print()
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
    true
  })

  
}