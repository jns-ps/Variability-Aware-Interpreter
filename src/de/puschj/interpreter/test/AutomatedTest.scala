package de.puschj.interpreter.test

import org.scalacheck._
import scala.collection.mutable.{Set => MSet}
import Gen._
import Arbitrary.arbitrary
import de.puschj.interpreter._
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Opt
import de.puschj.interpreter.Store
import de.fosd.typechef.featureexpr.FeatureExprFactory
import java.io.File

object InterpreterAutoCheck extends Properties("Interpreter") {
  
//    FeatureExprFactory.setDefault(FeatureExprFactory.bdd);
  
  // constraints
  final val FEATURENAMES = List("A","B","C","D","E","F")
  final val VARNAMES = List("a", "b", "c", "d", "e")
  
  
  // FeatureExpressions
  val genAtomicFeatureExpression =
        oneOf(FEATURENAMES.map(createDefinedExternal(_)))
            
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
    else Gen.frequency( (1, genAtomicFeatureExpression), (1, genCompoundFeatureExpr(size/2) ) )
  }
  
  def genFeatureExpr() = Gen.sized(size => Gen.frequency( (1, genFeatureExprSized(size)), (2, True) )) suchThat ( _ != False )
  
  implicit def arbFeatureExpression: Arbitrary[FeatureExpr] = Arbitrary {
    genFeatureExpr
  }
  

  // === Expressions === 
  val genStoreVarName = oneOf(VARNAMES)
        
  def genAtomicExpression() = {
    val genId = for {
      name <- genStoreVarName
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
	    
	  val genDiv = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Div(left,right)
    
	  Gen.oneOf(genAdd, genSub, genMul, genDiv) 
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
  } yield GreaterOE(left, right)
  
 val genLT = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield LessThan(left, right)
  
  val genLOE = for {
    left <- lzy(genExpression)
    right <- genExpression
  } yield LessOE(left, right)
  
  def genCondition = oneOf(genEQ, genGT, genGOE, genLT, genLOE)
  
  // === Statements ===
  val genAssignment = for {
    name <- genStoreVarName
    value <- genExpression
  } yield Assignment(name, value)
  
  val genWhile = for {
    cond <- genCondition
    stmt <- genStatement
  } yield While(cond, stmt)
  
  val genIf = for {
    cond <- genCondition
    ifbranch <- genStatement
    stmt <- genStatement
    elsebranch <- oneOf(None, Some(stmt))
  } yield If(cond, ifbranch, elsebranch)
  
  val genStatement: Gen[Statement] = Gen.frequency( (2, genAssignment), (1, genWhile), (1, genIf) )
  
  def genOptStatement: Gen[Opt[Statement]] = for {
    feat <- genFeatureExpr
    stmt <- genStatement
  } yield Opt(feat, stmt)
  
  def genProgram: Gen[VariableProgram] = Gen.sized( size => for {
    stmts <- listOfN(size, genOptStatement)
  } yield VariableProgram(stmts) )
  
//  implicit def arbProgram: Arbitrary[VariableProgram] = Arbitrary {
//    genProgram
//  }
  
  implicit def arbNonExceedingProgram: Arbitrary[VariableProgram] = Arbitrary {
    genProgram suchThat (_.runLoopCheck(new Store, new FuncStore) != false)
  }
  
  def saveProgramToFile(p: VariableProgram) = {
    val pw = new java.io.PrintWriter(new File("testprograms\\test"+(if (n<10) "0"+n else n)+".txt"))
    try {
      pw.println(p)
    } 
    finally { 
      pw.close()
      n += 1
    }
  }
  
  def saveAST_toFile(p: VariableProgram) = {
    val pw = new java.io.PrintWriter(new File("testprograms\\ast"+(if (n<10) "0"+n else n)+".txt"))
    try {
      pw.println(p.toStringAST)
    } 
    finally { 
      pw.close()
    }
  }
  
  var n = 0
  
  property("createTestCases") = Prop.forAll( (p: VariableProgram) => {
    saveAST_toFile(p)
    saveProgramToFile(p)
    
//    println("=== VARIABLE ===")
//    p.print()
//    println("================")
    true
  })
  
//  property("FeatureExpressions") = Prop.forAll( (f: FeatureExpr) => {
//    println(f)
//    true
//  })
  
//  property("configuredPrograms") = Prop.forAll( (p: VariableProgram) => ProgramUtils.compareProgramVariants(p, FEATURENAMES.toSet, VARNAMES.toSet) )

  
}