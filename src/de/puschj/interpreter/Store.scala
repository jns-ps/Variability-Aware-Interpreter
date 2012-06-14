package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.ConditionalLib.findSubtree
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One

class Store {
  
  private val entries: Map[String,Conditional[Value]] = Map.empty[String,Conditional[Value]]

  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "Store" else headline
    println("====== " + s + " =======")
    println(entries.toString())
    println("=======" + ("=" * s.length()) + "========")
    println()
  }
  
  def put(key: String, value: Conditional[Value]) = entries.put(key, value)
  
  def get(key: String) = {
    if (!entries.contains(key))
      One(UndefinedValue(key+" not initialized."))
    else
      entries.get(key).get
  }
  
  def getByContext(key: String, context: FeatureExpr) = {
    if (!entries.contains(key))
      One(UndefinedValue(key+" not initialized."))
    else
      findSubtree(context, entries.get(key).get)
  }
  
  def getStoredVariables() = entries.keySet
}


class FuncStore {
  
  private val functions: Map[String, FunctionDef] = Map.empty[String, FunctionDef]
  
  def put(funcName: String, funcDef: FunctionDef) = functions.put(funcName, funcDef)
  
  @throws(classOf[NoSuchMethodException])
  def get(funcName: String) = {
    if (!functions.contains(funcName))
      throw new NoSuchMethodException("No method with name: "+funcName)
    else
      functions.get(funcName).get
  }
  
}