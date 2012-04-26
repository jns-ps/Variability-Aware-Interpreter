package de.puschj.interpreter

import de.fosd.typechef.conditional.ConditionalMap

class Environment extends ConditionalMap[String,Int] {

  def print() {
    println("====== Environment =======")
    println(toString())
    println("==========================")
  }
}