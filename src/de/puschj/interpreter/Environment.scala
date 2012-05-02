package de.puschj.interpreter

import scala.collection.mutable.Map
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.conditional.Conditional
import scala.collection.mutable.HashMap

class Environment {
  
  val entries: Map[String,Conditional[Int]] = new HashMap[String,Conditional[Int]]()

  def print() {
    println("====== Environment =======")
    println(entries.toString())
    println("==========================")
  }
  
  def put(key: String, value: Conditional[Int]): Unit = {
    entries.put(key, value)
  }
  
  @throws(classOf[NoSuchElementException])
  def get(key: String): Conditional[Int] = {
    if (!entries.contains(key))
      throw new NoSuchElementException("no value stored for "+key)
    return entries.get(key).get
  }
}