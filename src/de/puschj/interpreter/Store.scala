package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One

class Store {
  
  val entries: Map[String,Conditional[Value]] = new HashMap[String,Conditional[Value]]()

  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "Store" else headline
    println("====== "+s+" =======")
    println(entries.toString())
    println("======="+ ("=" * s.length()) +"========")
    println()
  }
  
  def put(key: String, value: Conditional[Value]): Unit = {
    entries.put(key, value)
  }
  
  def get(key: String): Conditional[Value] = {
    if (!entries.contains(key))
      return One(UndefinedValue(key+" not yet initialized."))
    return entries.get(key).get
  }
}