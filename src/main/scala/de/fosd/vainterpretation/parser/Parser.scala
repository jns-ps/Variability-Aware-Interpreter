package de.fosd.vainterpretation.parser

import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.java15.JavaLexer
import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.parser.~
import de.fosd.typechef.parser.MultiFeatureParser
import de.fosd.vainterpretation.interpreter.Add
import de.fosd.vainterpretation.interpreter.And
import de.fosd.vainterpretation.interpreter.Assert
import de.fosd.vainterpretation.interpreter.Assign
import de.fosd.vainterpretation.interpreter.Block
import de.fosd.vainterpretation.interpreter.Bool
import de.fosd.vainterpretation.interpreter.Call
import de.fosd.vainterpretation.interpreter.Div
import de.fosd.vainterpretation.interpreter.Eq
import de.fosd.vainterpretation.interpreter.Expr
import de.fosd.vainterpretation.interpreter.FuncDec
import de.fosd.vainterpretation.interpreter.GoE
import de.fosd.vainterpretation.interpreter.GrT
import de.fosd.vainterpretation.interpreter.If
import de.fosd.vainterpretation.interpreter.LeT
import de.fosd.vainterpretation.interpreter.LoE
import de.fosd.vainterpretation.interpreter.Mul
import de.fosd.vainterpretation.interpreter.NEq
import de.fosd.vainterpretation.interpreter.Neg
import de.fosd.vainterpretation.interpreter.Null
import de.fosd.vainterpretation.interpreter.Num
import de.fosd.vainterpretation.interpreter.Or
import de.fosd.vainterpretation.interpreter.Par
import de.fosd.vainterpretation.interpreter.Stmt
import de.fosd.vainterpretation.interpreter.Sub
import de.fosd.vainterpretation.interpreter.Var
import de.fosd.vainterpretation.interpreter.VariableProgram
import de.fosd.vainterpretation.interpreter.While


class WhileParser extends MultiFeatureParser() {
    type Elem = TokenWrapper
    type TypeContext = Null
    
    def identifierToken: MultiParser[Elem] = token("identifier", x=>x.getText().matches("""[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""))
    
    implicit def textToken(t: String): MultiParser[Elem] =
      token(t, _.getText == t)  
      
    def intToken : MultiParser[Elem] =
      token("integer", x=>x.getText().matches("""([1-9][0-9]*)|0"""))
    
//    lazy val start: MultiParser[List[Opt[Statement]]] = "begin" ~> repOpt(declaration) ~ repOpt(statement) <~ "end" ^^ {
//      case decls~stmts => decls ++ stmts
//    }
      
    lazy val integer = intToken ^^ { x => Integer.parseInt(x.getText()) }
    
    lazy val identifier = identifierToken ^^ { x => x.getText() }
    
    lazy val nullExpr = "null" ^^ { x => Null }
    
    lazy val program = "begin" ~> (repOpt(decl | stmt)) <~ "end"
    
// =====================
// Statements
// =====================
    
    lazy val stmt: MultiParser[Stmt] = assert | assign | whileStmt | block | ifStmt
    
    lazy val assign = identifier ~ "=" ~ (expr !) ~ ";" ^^ {
      case n~_~e~_ => new Assign(n, e)
    }
    lazy val block = "{" ~ (stmt *) ~ "}" ^^ {
      case _~stmtlst~_ => Block(stmtlst) 
    }
    lazy val whileStmt = "while" ~ "(" ~ (expr !) ~ ")" ~ block ^^ { 
      case _~_~c~_~b =>  While(c, b) 
    }
    lazy val ifStmt = "if" ~> ("(" ~> (expr !) <~ ")") ~ block ~ (("else" ~> block) ?) ^^ {
      case c~thn~els => If(c,thn,els)
    } 
    lazy val assert = "assert" ~> "(" ~> expr  <~ ")" <~ ";" ^^ {
      case c => Assert(c)
    }
    lazy val decl = funcDecl
    
    lazy val funcDecl = "def" ~> identifier ~ ("(" ~> repSep(identifier, ",") <~ ")") ~ block ^^ {
      case funcName~funcArgs~funcBody => FuncDec(funcName, funcArgs, funcBody)
    }


// =====================
// Expressions
// =====================

    lazy val expr: MultiParser[Expr] = cond_3
    
    lazy val cond_3: MultiParser[Expr] = cond_2 ~ repPlain("&&" ~ cond_2 | "||" ~ cond_2) ^^ reduceList
    
    lazy val cond_2: MultiParser[Expr] = cond_1 ~ repPlain("==" ~ cond_1 | "!=" ~ cond_1) ^^ reduceList
    
    lazy val cond_1: MultiParser[Expr] = arith_2 ~ repPlain("<" ~ arith_2 | "<=" ~ arith_2 | ">" ~ arith_2 | ">=" ~ arith_2) ^^ reduceList
      
    lazy val arith_2: MultiParser[Expr] = arith_1 ~ repPlain("+" ~ arith_1 | "-" ~ arith_1) ^^ reduceList
    
    lazy val arith_1: MultiParser[Expr] = negation ~ repPlain("*" ~ negation | "/" ~ negation) ^^ reduceList
    
    lazy val negation: MultiParser[Expr] = (("!")?) ~ access ^^ {
      case None ~ e => e
      case _ ~ e => Neg(e)
    } 
  
    lazy val access: MultiParser[Expr] = factor ~ repPlain("." ~ (call | (identifier ^^ { case s => Var(s) })) ) ^^ reduceList
    
    lazy val factor: MultiParser[Expr] = 
      parenthesis |
      call |
      literal |
      nullExpr |
      integer ^^ { case i => Num(i) } |
      identifier ^^ { case i => Var(i) }
      
    lazy val literal: MultiParser[Expr] = ("false" | "true") ^^ {
      case b => Bool(b.getText.toBoolean)
    }
      
    lazy val call : MultiParser[Call] = identifier ~ ("(" ~> repSep(expr, ",") <~ ")")  ^^ {
      case name~args => Call(name, args)
    }
      
    lazy val parenthesis : MultiParser[Par] = "(" ~> (expr) <~ ")" ^^ {
      e => Par(e)
    }
    
    val reduceList: Expr ~ List[Elem ~ Expr] => Expr = {
      case i ~ ps => (i /: ps)(reduce) 
    }
  
    def reduce(l: Expr, r: Elem ~ Expr) = r._1.getText() match {
      case "+" => Add(l, r._2)
      case "-" => Sub(l, r._2)
      case "*" => Mul(l, r._2)
      case "/" => Div(l, r._2)
      
      case "<" => LeT(l, r._2)
      case "<=" => LoE(l, r._2)
      case ">" => GrT(l, r._2)
      case ">=" => GoE(l, r._2)
      case "==" => Eq(l, r._2)
      case "!=" => NEq(l, r._2)
      case "&&" => And(l, r._2)
      case "||" => Or(l, r._2)

    }

    def parse(code:String): VariableProgram = {
      val parser = new WhileParser()
      val y = parser.program(JavaLexer.lex(code), FeatureExprFactory.True)
      y match{
        case parser.Success(v,_) => {
            val p = new VariableProgram(v)
//            println(p)
            return p
        }
        case e: parser.NoSuccess =>
            throw new IllegalArgumentException("Bad syntax: "+code)
      }
    }
    
    def parseFile(filename: String): VariableProgram = {
      val source = scala.io.Source.fromFile(filename)
      val input = source.mkString
      source.close()
      return parse(input)
    }
}
