package de.puschj.interpreter

import de.fosd.typechef.conditional.Opt
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False}


sealed abstract class Program {
  
  def isEmpty(): Boolean

  def run(): Store[_]
 
  override def toString() = SourceCodePrettyPrinter.prettyPrint(this)
  
  def print() = println(toString)
  
  def toStringAST() : String
  
  def printAST() = println(toStringAST)
}

case class VariableProgram(private val stmts: List[Opt[Stmt]]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(): VAStore = run(new VAStore, new VAFuncStore, new VAClassStore)
  
  def run(store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): VAStore = {
    for(stm <- stmts) 
      try {
        VAInterpreter.execute(stm.entry, stm.feature, store, funcStore, classStore)
      }
      catch {
        case e: LoopExceededException => println(e.toString)
      }
    return store
  }
  
  def runLoopCheck(): Boolean = runLoopCheck(new VAStore, new VAFuncStore, new VAClassStore)
  
  def runLoopCheck(store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Boolean = {
    for(stm <- stmts)
      try {
        VAInterpreter.execute(stm.entry, stm.feature, store, funcStore, classStore)
      }
      catch {
        case e: LoopExceededException => return false
      }
    return true
  }
  
  def configured(selectedFeatures: Set[String]): ConfiguredProgram = {
    ConfiguredProgram(filterStatements(stmts, selectedFeatures).map(_.entry))
  }
  
  private def filterStatements(stmts: List[Opt[Stmt]], selectedFeatures: Set[String]): List[Opt[Stmt]] = {
    stmts.filter(_.feature.evaluate(selectedFeatures))
         .map(s => Opt(True, filterStatement(s.entry, selectedFeatures)))
  }
  
  private def filterStatement(stmt: Stmt, selectedFeatures: Set[String]): Stmt = {
      stmt match {
            case ExprStmt(expr) => ExprStmt(filterExpression(expr, selectedFeatures))
        
            case Assert(expr) => Assert(filterExpression(expr, selectedFeatures))
            
            case Assign(expr, value) => Assign(filterExpression(expr, selectedFeatures), 
                                                       filterExpression(value, selectedFeatures))
        
            case Block(stmts) => Block(filterStatements(stmts, selectedFeatures))
            case If(cond, thenB, elseB) => If(cond, Block(filterStatements(thenB.stmts, selectedFeatures)), 
                                             if (elseB.isDefined)
                                               Some(Block(filterStatements(elseB.get.stmts, selectedFeatures))) 
                                             else None)
            case While(cond, body) => While(filterExpression(cond, selectedFeatures), 
                                            Block(filterStatements(body.stmts, selectedFeatures)))
              
            case FuncDec(name, args, body) => FuncDec(name, 
                                                      args.filter(_.feature.evaluate(selectedFeatures))
                                                          .map(a => Opt(True, a.entry)), 
                                                      Block(filterStatements(body.stmts, selectedFeatures)))
            
            case ClassDec(name, args, superClass, optConsts, optFields, optFuncDecs) =>
              val filteredArgs = args.filter(_.feature.evaluate(selectedFeatures))
                                     .map(a => Opt(True, a.entry))
              val filteredConsts = filterStatements(optConsts, selectedFeatures).asInstanceOf[List[Opt[Assign]]]
              val filteredFields = filterStatements(optFields, selectedFeatures).asInstanceOf[List[Opt[Assign]]]
              val filteredFuncDecs = filterStatements(optFuncDecs, selectedFeatures).asInstanceOf[List[Opt[FuncDec]]]
              ClassDec(name, filteredArgs, superClass, filteredConsts, filteredFields, filteredFuncDecs)
          }
  }
  
  private def filterExpression(expr: Expr, selectedFeatures: Set[String]): Expr = {
    expr match {
      case Par(e) => Par(filterExpression(e, selectedFeatures))
      case Neg(e) => Neg(filterExpression(e, selectedFeatures))
      
      case Add(e1, e2) => Add(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case Sub(e1, e2) => Sub(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case Mul(e1, e2) => Mul(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case Div(e1, e2) => Div(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case Eq(e1, e2)  => Eq (filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case NEq(e1, e2) => NEq(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case GrT(e1, e2) => GrT(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case LeT(e1, e2) => LeT(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case GoE(e1, e2) => GoE(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case LoE(e1, e2) => LoE(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case And(e1, e2) => And(filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      case Or(e1, e2)  => Or (filterExpression(e1, selectedFeatures), filterExpression(e2, selectedFeatures))
      
      case Call(name, args) => Call(name, filterExpressions(args, selectedFeatures))
      case New(name, args) => New(name, filterExpressions(args, selectedFeatures))
      case MethodCall(expr, call) => MethodCall(filterExpression(expr, selectedFeatures), 
                                                filterExpression(call, selectedFeatures).asInstanceOf[Call])
      case e => e
    }
  }
  
  private def filterExpressions(exprs: List[Opt[Expr]], selectedFeatures: Set[String]): List[Opt[Expr]] = {
    exprs.filter(_.feature.evaluate(selectedFeatures))
         .map(e => Opt(True, filterExpression(e.entry, selectedFeatures)))
  }
  
  def toStringAST() = stmts.toString //println(ASTPrettyPrinter.prettyPrint(this))
}



case class ConfiguredProgram(private val stmts: List[Stmt]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(): PlainStore = run(new PlainStore, new PlainFuncStore, new PlainClassStore)
  
  def run(store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): PlainStore = {
    for(stm <- stmts) PlainInterpreter.execute(stm, store, funcStore, classStore)
    return store
  }
  
  def toStringAST() = stmts.toString //println(ASTPrettyPrinter.prettyPrint(this))
}