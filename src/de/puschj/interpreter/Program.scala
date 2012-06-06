package de.puschj.interpreter

import de.fosd.typechef.conditional.Opt
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False}


sealed abstract class Program {
  
  def isEmpty(): Boolean
  
  def run(store: Store): Store
  
  def print() = println(ASTPrettyPrinter.prettyPrint(this))
  
  override def toString() = ASTPrettyPrinter.prettyPrint(this)
}

case class VariableProgram(private val stmts: List[Opt[Statement]]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(store: Store): Store = {
    for(stm <- stmts) Interpreter.execute(stm.entry, stm.feature, store)
    return store
  }
  
  def configured(selectedFeatures: Set[String]): Program = {
    var filtered: ListBuffer[Statement] = ListBuffer.empty[Statement]
    for(stm <- stmts) {
      if (stm.feature.evaluate(selectedFeatures))
        filtered.append(stm.entry)
    }
    ConfiguredProgram(filtered.toList)
  }
}

case class ConfiguredProgram(private val stmts: List[Statement]) extends Program {
  
  def isEmpty() = stmts.isEmpty
  
  def getStatements() = stmts
  
  def run(store: Store): Store = {
    for(stm <- stmts) Interpreter.execute(stm, True, store)
    return store
  }
}