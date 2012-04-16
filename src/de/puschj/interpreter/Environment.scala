package de.puschj.interpreter

class Environment {
  var memory = scala.collection.mutable.Map[String, Int]()
  
  def put(name: String, value: Int) = memory.put(name, value);
  def get(name: String) : Int = return memory(name)
  
  def print() {
    println("====== Environment =======")
    memory foreach (println(_))
    println("==========================")
  }
}