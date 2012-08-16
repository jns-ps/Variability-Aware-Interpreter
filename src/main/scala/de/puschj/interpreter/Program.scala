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

case class VariableProgram(private val stmts: List[Opt[Statement]]) extends Program {
  
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
    var filtered: ListBuffer[Statement] = ListBuffer.empty[Statement]
    for(stm <- filterStatements(stmts, selectedFeatures)) {
      assert(stm.feature == True)
      filtered += stm.entry
    }
    ConfiguredProgram(filtered.toList)
  }
  
  private def filterStatements(stmts: List[Opt[Statement]], selectedFeatures: Set[String]): List[Opt[Statement]] = {
    var filtered: ListBuffer[Opt[Statement]] = ListBuffer.empty[Opt[Statement]]
    for(optstm <- stmts) {
      val feature = optstm.feature
      val stm = optstm.entry
      if (feature.evaluate(selectedFeatures))
          stm match {
            case Block(stmlist) =>
              filtered += Opt(True, Block(filterStatements(stmlist, selectedFeatures)))
            case If(cond, thenB, elseB) => 
              filtered += Opt(True, If(cond, Block(filterStatements(thenB.stmts, selectedFeatures)), 
                                         if (elseB.isDefined)
                                           Some(Block(filterStatements(elseB.get.stmts, selectedFeatures))) 
                                         else None))
            case While(cond, body) =>
              filtered += Opt(True, While(cond, Block(filterStatements(body.stmts, selectedFeatures))))
                
            case s =>
              filtered += Opt(True, s)
          }
    }
    filtered.toList
  }
  
  def toStringAST() = stmts.toString //println(ASTPrettyPrinter.prettyPrint(this))
}



case class ConfiguredProgram(private val stmts: List[Statement]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(): PlainStore = run(new PlainStore, new PlainFuncStore, new PlainClassStore)
  
  def run(store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): PlainStore = {
    for(stm <- stmts) PlainInterpreter.execute(stm, store, funcStore, classStore)
    return store
  }
  
  def toStringAST() = stmts.toString //println(ASTPrettyPrinter.prettyPrint(this))
}