package de.puschj.interpreter
import scala.text.Document
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.conditional.One
import de.fosd.typechef.conditional.Choice

//object ASTPrettyPrinter {
//  
//  private implicit def toDoc(node: ASTNode): Doc = prettyPrintNode(node)
//  private implicit def string(s: String): Doc = Text(s)
//  
//  def prettyPrint(prgm: Program): String = {
//    if (prgm.isEmpty) return ""
//    var doc: Doc = Empty
//    
//    prgm match {
//      case p: VariableProgram => 
//        for (optStmt <- p.getStatements()) {
//	      doc = doc <~ prettyPrintStatement(optStmt.entry, optStmt.feature)
//	    }
//      case p: ConfiguredProgram =>
//	    for (stmt <- p.getStatements()) {
//	      doc = doc <~ prettyPrintStatement(stmt, null)
//	    }
//    }
//    doc = "Program" ~~> doc
//    doc.mkString
//  }    
//  
//  def prettyPrintStatement(statement: Statement, feature: FeatureExpr): Doc = {
//     var doc: Doc = Empty
//     var stmtDoc: Doc =      
//         statement match {
//            case While(cond, stmt) => prettyPrintNode(cond) ~ prettyPrintStatement(stmt)
//         }
//     
//     if (feature != null) 
//       doc = doc ~ "Opt(" ~ feature.toString() ~ ")" /~ stmtDoc
//     else 
//       doc = stmtDoc
//
//    statement.getClass().getCanonicalName() ~~> doc  
//  }
//  
//  private def prettyPrintNode(node: ASTNode): Doc = {
//    node match {
//      // Statements
//      case Assignment(name, expr) => name ~~ "=" ~~ expr ~ ";"
//      case While(cond, stmt) => "while" ~ "(" ~cond ~ ")" ~> stmt
//      case Block(stmts) => {
//        if (stmts.isEmpty) Empty
//        else {
//	       var doc: Doc = Empty
//	       for (optStmt <- stmts) {
//	         doc = doc ~ prettyPrintFeatureExpr(optStmt.feature) <~ prettyPrintNode(optStmt.entry) ~ prettyPrintFeatureExprClose(optStmt.feature)
//	       }
//	       doc
//        }
//      }
//      case If(cond, s1, s2) => "if" ~ "(" ~ cond ~ ")" ~> s1 ~ (
//                                  if(s2.isDefined) Line ~ "else" ~> s2.get
//                                  else Empty
//                                )
//      case Assert(cond) => "assert" ~ "(" ~ cond ~ ")" ~ ";"
//
//      // Expressions                          
//      case Num(x) => x.toString()
//      case Id(varname) => varname
//      case Add(e1, e2) => e1 ~~ "+" ~~ e2
//      case Sub(e1, e2) => e1 ~~ "-" ~~ e2
//      case Mul(e1, e2) => e1 ~~ "*" ~~ e2
//      case Div(e1, e2) => e1 ~~ "/" ~~ e2
//      case Parens(expr) => "(" ~ expr ~ ")"
//      
//      //Conditions
//      case Equal(e1, e2) => e1 ~~ "==" ~~ e2
//      case GreaterThan(e1, e2) => e1 ~~ ">" ~~ e2
//      case GreaterOE(e1, e2) => e1 ~~ ">=" ~~ e2
//      case LessThan(e1, e2) => e1 ~~ "<" ~~ e2
//      case LessOE(e1, e2) => e1 ~~ "<=" ~~ e2
//      case Neg(cond) => "!" ~ "(" ~ cond ~ ")"
//      
//      case node => node.toString()
//    }
//  }
//  
//  private def prettyPrintFeatureExpr(feature: FeatureExpr): Doc = {
//    if (feature.isTautology()) Empty else Line ~ "//#ifdef " ~ feature.toString()
//  }
//  
//  private def prettyPrintFeatureExprClose(feature: FeatureExpr): Doc = {
//    if (feature.isTautology()) Empty else Line ~ "//#endif "
//  }
//}



object SourceCodePrettyPrinter {
  
  private implicit def toDoc(node: ASTNode): Doc = prettyPrintNode(node)
  private implicit def string(s: String): Doc = Text(s)
  
  def prettyPrint(prgm: Program): String = {
    var doc: Doc = Empty
    
    prgm match {
      case p: VariableProgram => 
        for (optStmt <- p.getStatements()) {
//          if (!optStmt.feature.isContradiction)
             doc = doc ~ prettyPrintFeatureExprOpen(optStmt.feature, true) <~ prettyPrintNode(optStmt.entry) ~ prettyPrintFeatureExprClose(optStmt.feature, true)
        }
      case p: ConfiguredProgram =>
        for (stmt <- p.getStatements()) {
          doc = doc <~ prettyPrintNode(stmt)
        }
    }
    doc = "begin" ~ doc <~ "end" 
    doc.mkString
  }
  
  def prettyPrintNode(node: ASTNode): Doc = {
    node match {
      // Statements
      case ExprStmt(expr) => expr ~ ";"
      case Assign(expr, value) => expr ~~ "=" ~~ prettyPrintConditional(value) ~ ";"
      case While(cond, stmt) => {
        var doc: Doc = "while" ~ "(" ~ prettyPrintConditional(cond) ~ ")"
        stmt match {
          case b: Block => doc ~~ stmt
          case s => doc ~> stmt
        }
      } 
      case Block(stmts) => {
        if (stmts.isEmpty) "{ }"
        else {
           var doc: Doc = Empty
           for (optStmt <- stmts) {
             doc = doc ~ prettyPrintFeatureExprOpen(optStmt.feature, true) <~ prettyPrintNode(optStmt.entry) ~ prettyPrintFeatureExprClose(optStmt.feature, true)
           }
           "{" ~ Nest(2, doc) <~ "}"
        }
      }
      case If(cond, s1, s2) => {
        var doc: Doc = "if" ~ "(" ~ prettyPrintConditional(cond) ~ ")"
        s1 match {
          case b: Block => doc = doc ~~ s1
          case s => doc = doc ~> s1
        }
        s2 match {
          case Some(b: Block) => doc ~~ "else" ~~ b
          case None => doc
        }      
      }
      case Assert(cond) => "assert" ~ "(" ~ cond ~ ")" ~ ";"
      case FuncDec(name, args, body) => {
        var doc = "def" ~~ name ~ "("
        for (i <- 0 until args.size) {
          doc = doc ~ prettyPrintFeatureExprOpen(args(i).feature, false) ~ args(i).entry ~ prettyPrintFeatureExprClose(args(i).feature, false)
          if (i < args.size - 1) {
            doc = doc ~ ", "
          }
        }
        doc ~ ")" ~~ body
      }
      
      case ClassDec(name, args, superClass, consts, fields, methods) => {
        var head = "class" ~~ name ~~ "("
        for (i <- 0 until args.size) {
          head = head ~ prettyPrintFeatureExprOpen(args(i).feature, false) ~ args(i).entry ~ prettyPrintFeatureExprClose(args(i).feature, false)
          if (i < args.size - 1) {
            head = head ~ ", "
          }
        }
        head = head ~ ")"
        
        var body: Doc = Empty
        for (i <- 0 until consts.size) {
          body = body <~ prettyPrintFeatureExprOpen(consts(i).feature, true) ~ "const" ~~ consts(i).entry ~ prettyPrintFeatureExprClose(consts(i).feature, true)
        }
        for (i <- 0 until fields.size) {
          body = body <~ prettyPrintFeatureExprOpen(fields(i).feature, true) ~ "var" ~~ fields(i).entry ~ prettyPrintFeatureExprClose(fields(i).feature, true)
        }
        for (i <- 0 until methods.size) {
          body = body <~ prettyPrintFeatureExprOpen(methods(i).feature, true) ~ methods(i).entry ~ prettyPrintFeatureExprClose(methods(i).feature, true)
        }
        
        head ~~ "{" ~> body <~ "}" 
      }

      // Expressions         
      case Null => "null"
      case Num(x) => x.toString()
      case Bool(b) => b.toString
      case Var(name) => name
      case Add(e1, e2) => e1 ~~ "+" ~~ e2
      case Sub(e1, e2) => e1 ~~ "-" ~~ e2
      case Mul(e1, e2) => e1 ~~ "*" ~~ e2
      case Div(e1, e2) => e1 ~~ "/" ~~ e2
      case Eq(e1, e2) => e1 ~~ "==" ~~ e2
      case NEq(e1, e2) => e1 ~~ "!=" ~~ e2
      case GrT(e1, e2) => e1 ~~ ">" ~~ e2
      case GoE(e1, e2) => e1 ~~ ">=" ~~ e2
      case LeT(e1, e2) => e1 ~~ "<" ~~ e2
      case LoE(e1, e2) => e1 ~~ "<=" ~~ e2
      case And(e1, e2) => e1 ~~ "&&" ~~ e2
      case Or(e1, e2) => e1 ~~ "||" ~~ e2
      case Neg(cond) => "!" ~ "(" ~ cond ~ ")"
      case Par(expr) => "(" ~ expr ~ ")"
      
      case Call(fname, args) => {
        var doc: Doc = fname ~ "("
         for (i <- 0 until args.size) {
          doc = doc ~ prettyPrintFeatureExprOpen(args(i).feature, false) ~ args(i).entry ~ prettyPrintFeatureExprClose(args(i).feature, false)
          if (i < args.size - 1) {
            doc = doc ~ ", "
          }
        }
        doc ~ ")"
      }
      case New(name, args) => {
        var doc = "new" ~~ name ~ "("
        for (i <- 0 until args.size) {
          doc = doc ~ prettyPrintFeatureExprOpen(args(i).feature, false) ~ args(i).entry ~ prettyPrintFeatureExprClose(args(i).feature, false)
          if (i < args.size - 1) {
            doc = doc ~ ", "
          }
        }
        doc ~ ")"
      }
      case Field(expr, name) => expr ~ "." ~ name
      case MethodCall(expr, call) => expr ~ "." ~ call
      
//      case node => node.toString()
    }
  }
  
  private def prettyPrintFeature(feature: FeatureExpr): Doc = 
      if (FeatureExprFactory.default == FeatureExprFactory.sat) {
        feature.toTextExpr
          .replace("!definedEx(", "(!")
          .replace("definedEx", "")
          .replace("0", "(A && !A)")
      }
      else {
        "("+
        feature.toTextExpr
          .replace("!definedEx(", "(!")
          .replace("definedEx", "")
          .replace("||", ") || (")
          .replace("0", "(A && (!A))") + ")"
      }
        
  
  
  private def prettyPrintFeatureExprOpen(feature: FeatureExpr, breakLine: Boolean): Doc = {
    if (feature.isTautology()) 
      Empty
    else {
      val doc: Doc = "//#if " ~ prettyPrintFeature(feature)     
        
      (if (breakLine) Line else Empty) ~ doc ~ " "
    }
  }
  
  private def prettyPrintFeatureExprClose(feature: FeatureExpr, breakLine: Boolean): Doc = {
    if (feature.isTautology()) 
      Empty 
    else {
      (if (breakLine) Line else Text(" ")) ~ "//#endif"
    }
  }
  
  def prettyPrintConditional(elem: Conditional[ASTNode]): Doc = {
    elem match {
      case One(node) => node
      case Choice(feature, thn, els) => "#if " ~ prettyPrintFeature(feature) ~> prettyPrintConditional(thn) <~ "#else" ~> prettyPrintConditional(els) <~ "#endif"
    }
  }
}


sealed abstract class Doc {
  def ~(that: Doc) = Cons(this, that)
  def ~~(that: Doc) = this ~ space ~ that
  def /~(that: Doc) = this ~ Text(",") <~ that
  def <~(that: Doc) = this ~ Line ~ that
  def ~>(that: Doc) = this ~ nest(2, Line ~ that)
  def ~~>(that: Doc) = this ~ block(that)
  
  def space = Text(" ")
  def nest(n: Int, d: Doc) = Nest(n, d)
  def block(d: Doc): Doc = Text("(") ~> d <~ Text(")")
  
  def mkString: String = this match {
    case Empty => ""
    case Line => "\n"
    case Text(s) => s
    case Cons(l, r) => l.mkString + r.mkString
    
    case Nest(n, Empty) => Empty.mkString
    case Nest(n, Line) => "\n" + (" " * n)
    case Nest(n, Text(s)) => (Text(s)).mkString
    case Nest(n, Cons(l, r)) => (Cons(Nest(n, l), Nest(n, r))).mkString
    case Nest(i, Nest(j, x)) => (Nest(i+j, x)).mkString
  }
}




case object Empty                         extends Doc
case object Line                          extends Doc
case class  Text  (s: String)             extends Doc
case class  Cons  (left: Doc, right: Doc) extends Doc
case class  Nest  (n: Int, d: Doc)        extends Doc
