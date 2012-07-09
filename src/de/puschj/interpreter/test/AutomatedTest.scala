package de.puschj.interpreter.test

import org.scalacheck._
import scala.collection.mutable.{Set => MSet}
import Gen._
import Arbitrary.arbitrary
import de.puschj.interpreter._
import de.puschj.interpreter.test.TestConstraints._
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Opt
import de.puschj.interpreter.VAStore
import de.puschj.interpreter.FileUtils._
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.puschj.parser.WhileParser
import java.io.File

object InterpreterAutoCheck extends Properties("Interpreter") {
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd);
  
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
  
  def genFeatureExpr() = Gen.sized(size => Gen.frequency( (1, genFeatureExprSized(size)), (2, True) ))
  
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
	  } yield Parens(Add(left,right))
	  
	  val genSub = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Parens(Sub(left,right))
	    
	  val genMul = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Parens(Mul(left,right))
	    
	  val genDiv = for {
	    left <- lzy(genExpressionSized(size))
	    right <- genExpressionSized(size)
	  } yield Parens(Div(left,right))
    
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
  
  def genBlock: Gen[Block] = for {
    n <- Gen.choose(1, 3)
    stmts <- listOfN(n, genOptStatementLessLoops)
  } yield Block(stmts)
  
  def genWhile: Gen[While] = for {
    cond <- genCondition
    block <- genBlock
  } yield While(cond, block)
  
  def genIf: Gen[If] = for {
    cond <- genCondition
    ifbranch <- genBlock
    stmt <- genBlock
    elsebranch <- oneOf(None, Some(stmt))
  } yield If(cond, ifbranch, elsebranch)
  
  def genOptStatement: Gen[Opt[Statement]] = for {
    feat <- genFeatureExpr
    stmt <- Gen.frequency( (3, genAssignment), (1, genWhile), (1, genIf) )
  } yield Opt(feat, stmt)
  
  // for generating less nested loops
  def genOptStatementLessLoops: Gen[Opt[Statement]] = for {
    feat <- genFeatureExpr
    stmt <- Gen.frequency( (10, genAssignment), (1, genWhile), (1, genIf) )
  } yield Opt(feat, stmt)
  
  def genProgram: Gen[VariableProgram] = Gen.sized( size => for {
    stmts <- listOfN(size, genOptStatement)
  } yield VariableProgram(stmts) )
  
//  implicit def arbProgram: Arbitrary[VariableProgram] = Arbitrary {
//    genProgram
//  }
  
  implicit def arbNonExceedingProgram: Arbitrary[VariableProgram] = Arbitrary {
    genProgram suchThat (_.runLoopCheck(new VAStore, new FuncStore))
  }
  
  var n = 0
  
  val parser = new WhileParser()
  
//  property("createTestCases") = Prop.forAll( (p: VariableProgram) => {
//    if (n == 0)
//        for (file <- new File("testprograms").listFiles)
//            file.delete
//    saveProgramAST(p, "testprograms\\ast"+(if (n<10) "0"+n else n)+".txt")
//    saveProgram(p, "testprograms\\test"+(if (n<10) "0"+n else n)+".txt")
//    println("TestCase "+ n +" created.")
//    n += 1
//    true
//  })
  
//  property("checkPrettyPrinter") = Prop.forAll( (p: VariableProgram) => {
//    val parsed = parser.parse(p.toString)
//    val gStore = p.run(new Store(), new FuncStore())
//    val pStore = parsed.run(new Store(), new FuncStore())
//    
//    if (!gStore.equals(pStore)) {
//      p.printAST
//      parsed.printAST
//      gStore.print("Generated Store")
//      pStore.print("Parsed Store")
//      false
//    } else
//      true
//  })
  
  property("configuredPrograms") = Prop.forAll( (p: VariableProgram) => {
    println("testing variable program "+n)
    n += 1
    ProgramUtils.compareProgramVariants(p, FEATURENAMES.toSet, VARNAMES.toSet) 
  })

  
}