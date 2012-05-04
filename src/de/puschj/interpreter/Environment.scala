package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import scala.collection.mutable.HashMap
import de.fosd.typechef.conditional.One

class Environment {
  
  val entries: Map[String,Conditional[Int]] = new HashMap[String,Conditional[Int]]()

  def print(headline: String = "") {
    val s: String = if (headline.isEmpty()) "Environment" else headline
    println("====== "+s+" =======")
    println(entries.toString())
    val sb: StringBuilder = new StringBuilder()
    for (i <- 1 to s.length()) sb.append("=")
    println("======="+sb.toString()+"========")
    println()
  }
  
  def put(key: String, value: Conditional[Int]): Unit = {
    entries.put(key, value)
  }
  
  @throws(classOf[NoSuchElementException])
  def get(key: String): Conditional[Int] = {
    if (!entries.contains(key))
      //throw new NoSuchElementException("no value stored for "+key)
      return One(0)
    return entries.get(key).get
  }
}