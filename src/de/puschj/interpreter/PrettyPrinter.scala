package de.puschj.interpreter
import scala.text.Document
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.sat.True

object ASTPrettyPrinter {
  
  private implicit def toDoc(node: ASTNode): Doc = prettyPrintNode(node)
  private implicit def string(s: String): Doc = Text(s)
  
  def prettyPrint(prgm: Program): String = {
    if (prgm.stmts.isEmpty) return ""
    var doc: Doc = prgm.stmts(0).entry
    for (optStmt <- prgm.stmts.tail) {
      doc = doc <~ prettyPrintNode(optStmt.entry) ~~ prettyPrintFeatureExpr(optStmt.feature)
    }
    layout("begin" ~ Line ~ doc ~ Line ~"end")
  }    
  
  private def prettyPrintNode(node: ASTNode): Doc = {
    node match {
      // Statements
      case Assignment(name, expr) => name ~~ "=" ~~ expr ~ ";"
      case While(cond, stmt) => "while" ~ "(" ~cond ~ ")" ~> stmt
      case Block(stmts) => {
        if (stmts.isEmpty) Empty
        else {
        var doc: Doc = stmts(0).entry
        for (optStmt <- stmts.tail) {
          doc = doc <~ prettyPrintNode(optStmt.entry)
        }
          doc
        }
      }
      case If(cond, s1, s2) => "if" ~ "(" ~ cond ~ ")" ~> s1 ~ (
                                  if(s2.isDefined) Line ~ "else" ~> s2.get
                                  else Empty
                                )
      case Assert(cond) => "assert" ~ "(" ~ cond ~ ")" ~ ";"

      // Expressions                          
      case Num(x) => x.toString()
      case Id(varname) => varname
      case Add(e1, e2) => e1 ~~ "+" ~~ e2
      case Sub(e1, e2) => e1 ~~ "-" ~~ e2
      case Mul(e1, e2) => e1 ~~ "*" ~~ e2
      case Div(e1, e2) => e1 ~~ "/" ~~ e2
      case Parens(expr) => "(" ~ expr ~ ")"
      
      //Conditions
      case Equal(e1, e2) => e1 ~~ "==" ~~ e2
      case GreaterThan(e1, e2) => e1 ~~ ">" ~~ e2
      case GreaterOE(e1, e2) => e1 ~~ ">=" ~~ e2
      case LessThan(e1, e2) => e1 ~~ "<" ~~ e2
      case LessOE(e1, e2) => e1 ~~ "<=" ~~ e2
      case Neg(cond) => "!" ~ "(" ~ cond ~ ")"
      
      case node => node.toString()
    }
  }
  
  def prettyPrintFeatureExpr(feature: FeatureExpr): Doc = {
    feature match {
      case fe => if (fe.isTautology()) Empty else "#" ~ fe.toString()
    }
  }
  
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
  def <~(that: Doc) = this ~ Line ~ that
  def ~>(that: Doc) = this ~ nest(2, Line ~ that)
  def ~~>(that: Doc) = this ~ block(that)
  
  def space = Text(" ")
  def nest(n: Int, d: Doc) = Nest(n, d)
  def block(d: Doc): Doc = Text("(") ~> d <~ Text(")")
}

case object Empty                         extends Doc
case object Line                          extends Doc
case class  Text  (s: String)             extends Doc
case class  Cons  (left: Doc, right: Doc) extends Doc
case class  Nest  (n: Int, d: Doc)        extends Doc
