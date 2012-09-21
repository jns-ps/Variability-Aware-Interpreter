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
import scala.collection.mutable.ListBuffer

object InterpreterAutoCheck extends Properties("Interpreter") {
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd);
  
  val parser = new WhileParser()
  
  
  // FeatureExpressions
  val genAtomicFeatureExpression =
        oneOf(FEATURENAMES.map(createDefinedExternal(_)))
            
  
  def genFeatureExpr() = {
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
        else Gen.frequency( (1, genAtomicFeatureExpression), (2, genCompoundFeatureExpr(size/2)), (1, True) )
      }
      sized( sz => {
        genFeatureExprSized(sz) 
      })
  }
  
  implicit def arbFeatureExpression: Arbitrary[FeatureExpr] = Arbitrary {
    genFeatureExpr
  }
  

  // === Expressions === 
  def genStoreVarName = oneOf(VARNAMES)
        
  def genAtomicExpression() = {
    def genId = for {
      name <- genStoreVarName
    } yield Id(name)
  
    def genNum = for {
      n <- Gen.choose(1, 100)
    } yield Num(n)
    
    oneOf(genId, genNum)
  } 
  
  def genCall(size: Int, funcNames: Seq[String]) = for {
    funcName <- oneOf(funcNames)
    arg1 <- genExpressionSized(size, funcNames)
    arg2 <- genExpressionSized(size, funcNames)
  } yield {
    val args = ListBuffer.empty[Opt[Expr]]
    args += Opt(True, arg1)
    if (predefFuncDefs.find(x => x.name equals funcName).get.args.size == 2)
      args += Opt(True, arg2)
    Call(funcName, args.toList)
  }
  
  def genCompoundExpression(size: Int, funcNames: Seq[String]) = {
	  val genAdd = for {
	    left <- lzy(genExpressionSized(size, funcNames))
	    right <- genExpressionSized(size, funcNames)
	  } yield Par(Add(left,right))
	  
	  val genSub = for {
	    left <- lzy(genExpressionSized(size, funcNames))
	    right <- genExpressionSized(size, funcNames)
	  } yield Par(Sub(left,right))
	    
	  val genMul = for {
	    left <- lzy(genExpressionSized(size, funcNames))
	    right <- genExpressionSized(size, funcNames)
	  } yield Par(Mul(left,right))
	    
	  val genDiv = for {
	    left <- lzy(genExpressionSized(size, funcNames))
	    right <- genExpressionSized(size, funcNames)
	  } yield Par(Div(left,right))
    
	  Gen.oneOf(genAdd, genSub, genMul, genDiv) 
  }
  
  def genExpressionSized(size: Int, funcNames: Seq[String]): Gen[Expr] = {
    if (size <= 0) genAtomicExpression
    else Gen.frequency( (5, genAtomicExpression), (1, genCompoundExpression(size / 2, funcNames)), (1, genCall(size / 2, funcNames)))
  }
  
  def genExpression(funcNames: Seq[String]) = Gen.sized(size => genExpressionSized(size, funcNames))
  
  // === Conditions ===
  def genEQ(funcNames: Seq[String]) = for {
    left <- lzy(genExpression(funcNames))
    right <- genExpression(funcNames)
  } yield Eq(left, right)
  
  def genGT(funcNames: Seq[String]) = for {
    left <- lzy(genExpression(funcNames))
    right <- genExpression(funcNames)
  } yield GrT(left, right)
  
 def genGOE(funcNames: Seq[String]) = for {
    left <- lzy(genExpression(funcNames))
    right <- genExpression(funcNames)
  } yield GoE(left, right)
  
 def genLT(funcNames: Seq[String]) = for {
    left <- lzy(genExpression(funcNames))
    right <- genExpression(funcNames)
  } yield LeT(left, right)
  
  def genLOE(funcNames: Seq[String]) = for {
    left <- lzy(genExpression(funcNames))
    right <- genExpression(funcNames)
  } yield LoE(left, right)
  
  def genCondition(funcNames: Seq[String]) = oneOf(genEQ(funcNames), genGT(funcNames), genGOE(funcNames), genLT(funcNames), genLOE(funcNames))
  
  // === Functions ===
  val predefFuncDefs = {
    val functionDeclarations = 
      "begin " +
      "def inc(x) { res = x + 1; } " +
      "def dec(x) { res = x - 1; } " +
      "def inv(x) { res = 0 - x; } " +
      "def abs(x) { if (x < 0) { res = 0 - x; } }" +
      "def mean(a,b) { res = (a + b) / 2; } " +
      "def sum(a,b)  { res = a + b; } " +
      "def sub(a,b)  { res = a - b; } " +
      "def mod(a,b)  { k = 0; while(a - k * b >= b) { k = k + 1; } res = a - k * b; } " +
      "def min(a,b)  { if (a <= b) { res = a; } else { res = b; } } " +
      "def max(a,b)  { if (a >= b) { res = a; } else { res = b; } } " +
      "end"
    val program = parser.parse(functionDeclarations)
    val funcDefs = ListBuffer.empty[FuncDec]
    for (stmtOpt <- program.getStatements)
      funcDefs += stmtOpt.entry.asInstanceOf[FuncDec]
    funcDefs.toList
  }
  
  def genOptFuncDefs(nDecs: Int): Gen[Seq[Opt[FuncDec]]] = for { 
    count <- choose(0, scala.math.min(nDecs, 10))
    funcDefs <- pick(count, predefFuncDefs)
    featExpressions <- listOfN(count, genFeatureExpr)
  } yield (featExpressions, funcDefs).zipped map (Opt(_,_))
    
  // === Statements ===
  def genAssignment(funcNames: Seq[String]) = for {
    name <- genStoreVarName
    value <- genExpression(funcNames)
  } yield Assign(Id(name), value)
  
  def genBlock(nested: Int, funcNames: Seq[String]): Gen[Block] = for {
    n <- Gen.choose(1, 3)
    stmts <- listOfN(n, genOptStatementNested(nested, funcNames))
  } yield Block(stmts)
  
  def genWhile(nested: Int, funcNames: Seq[String]): Gen[While] = for {
    cond <- genCondition(funcNames)
    block <- genBlock(nested + 1, funcNames)
  } yield While(cond, block)
  
  def genIf(nested: Int, funcNames: Seq[String]): Gen[If] = for {
    cond <- genCondition(funcNames)
    ifbranch <- genBlock(nested, funcNames)
    stmt <- genBlock(nested, funcNames)
    elsebranch <- oneOf(None, Some(stmt))
  } yield If(cond, ifbranch, elsebranch)
  
  def genOptStatementTopLevel(funcNames: Seq[String]): Gen[Opt[Stmt]] = for {
    feat <- genFeatureExpr
    stmt <- Gen.frequency( (3, genAssignment(funcNames)), (1, genWhile(0, funcNames)), (1, genIf(0, funcNames)) )
  } yield Opt(feat, stmt)
  
  def genStatementNested(nested: Int, funcNames: Seq[String]): Gen[Stmt] = {
    if (nested > 2) 
      Gen.frequency( (10, genAssignment(funcNames)), (1, genIf(nested, funcNames)) )
    else
      Gen.frequency( (10, genAssignment(funcNames)), (1, genWhile(nested, funcNames)), (1, genIf(nested, funcNames)) )
  }
  
  def genOptStatementNested(nested: Int, funcNames: Seq[String]): Gen[Opt[Stmt]] = for {
    feat <- genFeatureExpr
    stmt <- genStatementNested(nested, funcNames)
  } yield Opt(feat, stmt)
  
  def genProgram(size: Int): Gen[VariableProgram] = {
//    println("generating program with size "+size)
    def nonExceeding = (  for {
                       decls <- genOptFuncDefs( size / 10 )
                       stmts <- listOfN(size, genOptStatementTopLevel(decls.map(x => x.entry.name)))
                     } yield VariableProgram(decls ++: stmts)
                  ) suchThat (_.runLoopCheck)
    val program = nonExceeding.sample
    if (!program.isDefined)
      genProgram(size)
    else
      program.get
  }
  
  implicit def arbProgram: Arbitrary[VariableProgram] = Arbitrary {
    sized( size => genProgram(size) )
  }
  
  var tcCount = 0
  
//  property("createTestCases") = Prop.forAll( (p: VariableProgram) => {
//    if (tcCount == 0)
//        for (file <- new File("testprograms").listFiles)
//            file.delete
////    saveProgramAST(p, "testprograms\\ast"+(if (tcCount<10) "0"+tcCount else tcCount)+".txt")
//    saveProgram(p, "testprograms\\test%02d.txt".format(tcCount))
//    println("TestCase "+ tcCount +" created.")
//    tcCount += 1
//    true
//  })
  
//  property("checkPrettyPrinter") = Prop.forAll( (p: VariableProgram) => {
//    val parsed = parser.parse(p.toString)
//    val gStore = p.run()
//    val pStore = parsed.run()
//    
//    if (!gStore.equals(pStore)) {
//      p.printAST
//      parsed.printAST
//      gStore.print("Generated Store")
//      pStore.print("Parsed Store")
//      false
//    } else {
//      println("PrettyPrinter check "+tcCount+" passed.")
//      tcCount += 1
//      true
//    }
//  })
  
  property("configuredPrograms") = Prop.forAll( (p: VariableProgram) => {
    println("testing variable program "+tcCount)
    tcCount += 1
    ProgramUtils.compareProgramVariants(p, FEATURENAMES.toSet, VARNAMES.toSet) 
  })
  
//  property("plainInterpreterSpeedUp") = Prop.forAll( (p: VariableProgram) => {
//    println("testing variable program "+tcCount)
//    tcCount += 1
//    var start = 0L
//    var stop = 0L
//    val conf = p.configured(FEATURENAMES.toSet)
//    start = System.nanoTime
//    p.run
//    stop = System.nanoTime
//    val vainterpr: Double = stop - start
//    println(vainterpr)
//    
//    start = System.nanoTime
//    conf.run
//    stop = System.nanoTime
//    val plaininterpr: Double = stop - start
//    println(stop - start)
//    println(vainterpr / plaininterpr)
//    println("==========================")
//    true
//  })

  
}