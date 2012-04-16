package de.puschj.interpreter


class PrettyPrinter extends Visitor {
  private val sb : StringBuilder = new StringBuilder()
  private def inc() = sb.append("   ")
  
  def visit(node: ASTNode) = 
    node match {
       case Assignment(name, value) => println(sb.toString()+"Assignment(\""+name+"\", "+value+")")
  }
}