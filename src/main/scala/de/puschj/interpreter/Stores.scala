package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.ConditionalLib
import de.fosd.typechef.conditional.ConditionalLib.findSubtree
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One
import de.fosd.typechef.conditional.Opt


// === stores for variables ===

abstract class Store[T] {
  
  protected val entries: Map[String, T] = Map.empty[String, T]
  
  def put(key: String, value: T) = entries.put(key, value)
  
  def get(key: String): T = {
    if (!entries.contains(key))
      undefined(key+" not initialized.")
    else
      entries.get(key).get
  }
  
  protected def undefined(s: String): T
  
  def getStoredVariables() = entries.keySet
  
  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "Store" else headline
    println("====== " + s + " =======")
    println(entries.toString())
    println("=======" + ("=" * s.length()) + "========")
    println()
  }
}


class PlainStore extends Store[Value] {
  
  def undefined(s: String) = UndefinedValue(s)
  
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


class VAStore extends Store[Conditional[Value]] {
  
  def undefined(s: String) = One(UndefinedValue(s))

  def getByContext(key: String, context: FeatureExpr) = {
    if (!entries.contains(key))
      undefined(key+" not initialized.")
    else
      findSubtree(context, entries.get(key).get)
  }

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

// === function stores ===

case class FunctionDef(args: List[String], body: Block)


class PlainFuncStore {
  
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

class VAFuncStore {
  private val functions: Map[String, Opt[FunctionDef]] = Map.empty[String, Opt[FunctionDef]]
  
  def put(key: String, funcDef: Opt[FunctionDef]) = functions.put(key, funcDef)
  
  @throws(classOf[NoSuchMethodException])
  def get(funcName: String) = {
    if (!functions.contains(funcName))
      throw new NoSuchMethodException("No method with name: "+funcName)
    else
      functions.get(funcName).get
  }
}