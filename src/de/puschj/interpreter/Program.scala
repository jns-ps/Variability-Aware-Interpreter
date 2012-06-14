package de.puschj.interpreter

import de.fosd.typechef.conditional.Opt
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False}


sealed abstract class Program {
  
  def isEmpty(): Boolean
  
  def run(store: Store, funcStore: FuncStore): Store
  
  def print() = println(SourceCodePrettyPrinter.prettyPrint(this))
  
  def printAST();
  
  override def toString() = SourceCodePrettyPrinter.prettyPrint(this)
}

case class VariableProgram(private val stmts: List[Opt[Statement]]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(store: Store, funcStore: FuncStore): Store = {
    for(stm <- stmts) 
      try {
        Interpreter.execute(stm.entry, stm.feature, store, funcStore)
      }
      catch {
        case e: LoopExceededException => println(e.toString)
      }
      
    return store
  }
  
  def runLoopCheck(store: Store, funcStore: FuncStore): Boolean = {
    var sto: Store = store
    for(stm <- stmts)
      try {
        Interpreter.execute(stm.entry, stm.feature, sto, funcStore)
      }
      catch {
        case e: LoopExceededException => return false
      }
    return true
  }
  
  def configured(selectedFeatures: Set[String]): ConfiguredProgram = {
    var filtered: ListBuffer[Statement] = ListBuffer.empty[Statement]
    for(stm <- stmts) {
      if (stm.feature.evaluate(selectedFeatures))
        filtered.append(stm.entry)
    }
    ConfiguredProgram(filtered.toList)
  }
  
  def printAST() = println(stmts) //println(ASTPrettyPrinter.prettyPrint(this))
}

case class ConfiguredProgram(private val stmts: List[Statement]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(store: Store, funcStore: FuncStore): Store = {
    for(stm <- stmts) Interpreter.execute(stm, True, store, funcStore)
    return store
  }
  
  def printAST() = println(stmts) //println(ASTPrettyPrinter.prettyPrint(this))
}