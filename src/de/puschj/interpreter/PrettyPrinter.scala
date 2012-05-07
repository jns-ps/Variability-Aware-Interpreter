package de.puschj.interpreter


class PrettyPrinter extends Visitor {  
  def toDoc(node: ASTNode): Doc = {
    node match {
      case Assignment(name, value) => Text("Assignment") ~ block(Text(""))
    }
  }
  
//  def visit(node: ASTNode) = 
//    node match {
//       case Assignment(name, value) => println(sb.toString()+"Assignment(\""+name+"\", "+value+")")
//  }
}

object PrettyPrintUtils {
  def layout(d: Doc): String = d match {
    case Empty => ""
    case Line => "\n"
    case Text(s) => s
    case Cons(l, r) => layout(l) + layout(r)
    
    case Nest(n, Empty) => layout(Empty)
    case Nest(n, Line) => "\n" + (" " * n)
    case Nest(n, Text(s)) => layout(Text(s))
    case Nest(n, Cons(l, r)) => layout(Cons(Nest(n, l), Nest(n, r)))
    case Nest(i, Nest(j, x)) => layout(Nest(i+j, x))
  }
}

sealed abstract class Doc {
  def ~(that: Doc) = Cons(this, that)
  def ~~(that: Doc) = this ~ space ~ that
  def <~(that: Doc) = this ~ line ~ that
  def ~>(that: Doc) = this ~ nest(2, line ~ that)
  
  implicit def string(s: String): Doc = Text(s)
  val line  = Line
  val space = Text(" ")
  def nest(n: Int, d: Doc) = Nest(n, d)
  def block(d: Doc): Doc = space ~ "(" ~> d <~ ")"
}

case object Empty                         extends Doc
case object Line                          extends Doc
case class  Text  (s: String)             extends Doc
case class  Cons  (left: Doc, right: Doc) extends Doc

case class  Nest  (n: Int, d: Doc)        extends Doc
