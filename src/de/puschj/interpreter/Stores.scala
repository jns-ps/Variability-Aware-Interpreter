package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.ConditionalLib
import de.fosd.typechef.conditional.ConditionalLib.findSubtree
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One

abstract class Store

class VAStore extends Store {
  
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
  
  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[VAStore])
        return false
    val s = that.asInstanceOf[VAStore]
    if (!getStoredVariables().equals(s.getStoredVariables()))
        return false
    for (variable <- getStoredVariables())
        if (!ConditionalLib.equals(entries.get(variable).get, s.entries.get(variable).get))
            return false
    true
  }
}

class PlainStore extends Store {
  
  private val entries: Map[String,Value] = Map.empty[String,Value]
  
  def put(key: String, value: Value) = entries.put(key, value)
  
  def get(key: String) = {
    if (!entries.contains(key))
      UndefinedValue(key+" not initialized.")
    else
      entries.get(key).get
  }
  
  def getStoredVariables() = entries.keySet
  
  override def equals(that: Any): Boolean = {
    if (!that.isInstanceOf[PlainStore])
        return false
    val s = that.asInstanceOf[PlainStore]
    if (!getStoredVariables().equals(s.getStoredVariables()))
        return false
    for (variable <- getStoredVariables())
        if (!entries.get(variable).equals(s.entries.get(variable).get))
            return false
    true
  }
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