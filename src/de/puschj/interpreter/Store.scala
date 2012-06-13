package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.ConditionalLib.findSubtree
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One

class Store {
  
  private val entries: Map[String,Conditional[Value]] = new HashMap[String,Conditional[Value]]()

  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "Store" else headline
    println("====== " + s + " =======")
    println(entries.toString())
    println("=======" + ("=" * s.length()) + "========")
    println()
  }
  
  def put(key: String, value: Conditional[Value]): Unit = {
    entries.put(key, value)
  }
  
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