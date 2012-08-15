package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False}
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.ConditionalLib
import de.fosd.typechef.conditional.ConditionalLib.findSubtree
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One
import de.fosd.typechef.conditional.Opt


// === stores for variables ===

sealed abstract class Store[T] {
  
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

sealed abstract class FuncStore[T] {
  
  protected val functions: Map[String, T] = Map.empty[String, T]
  
  def put(funcName: String, funcDef: T) = functions.put(funcName, funcDef)
  
  protected def undefined(s: String): T
  
  def get(funcName: String): T = {
    if (!functions.contains(funcName))
      undefined("func \""+funcName+"\" not declared")
    else
      functions.get(funcName).get
  }
  
  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "FuncStore" else headline
    println("====== " + s + " =======")
    println(functions.toString)
    println("=======" + ("=" * s.length()) + "========")
    println()
  }
}

class PlainFuncStore extends FuncStore[FuncDef] {
    def undefined(s: String) = FErr(s)
}

class VAFuncStore extends FuncStore[Conditional[FuncDef]] {
    def undefined(s: String) = One(FErr(s))
}

// === stores for classes ===

sealed abstract class ClassStore[T] {
  
  protected val classes: Map[String, T] = Map.empty[String, T]
  
  def put(funcName: String, funcDef: T) = classes.put(funcName, funcDef)
  
  def get(funcName: String): T = {
    if (!classes.contains(funcName))
      undefined("func \""+funcName+"\" not declared")
    else
      classes.get(funcName).get
  }
  
  protected def undefined(s: String): T
  
  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "ClassStore" else headline
    println("====== " + s + " =======")
    println(classes.toString)
    println("=======" + ("=" * s.length()) + "========")
    println()
  }
}

class PlainClassStore extends ClassStore[ClassDef] {
    classes.put("Object", CDef("", List.empty[Opt[Assignment]], List.empty[Opt[FuncDec]]))
  
    def undefined(s: String) = CErr(s)
}

class VAClassStore extends ClassStore[Conditional[ClassDef]] {
    classes.put("Object", One(CDef("", List.empty[Opt[Assignment]], List.empty[Opt[FuncDec]])))
  
    def undefined(s: String) = One(CErr(s))
}