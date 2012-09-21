package de.puschj.parser

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.parser.java15.JavaLexer
import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.parser.~
import de.fosd.typechef.parser.MultiFeatureParser
import de.puschj.interpreter.Add
import de.puschj.interpreter.And
import de.puschj.interpreter.Assert
import de.puschj.interpreter.Assign
import de.puschj.interpreter.Block
import de.puschj.interpreter.Bool
import de.puschj.interpreter.Call
import de.puschj.interpreter.ClassDec
import de.puschj.interpreter.Div
import de.puschj.interpreter.Eq
import de.puschj.interpreter.Expr
import de.puschj.interpreter.ExprStmt
import de.puschj.interpreter.Field
import de.puschj.interpreter.FuncDec
import de.puschj.interpreter.GoE
import de.puschj.interpreter.GrT
import de.puschj.interpreter.Id
import de.puschj.interpreter.If
import de.puschj.interpreter.LeT
import de.puschj.interpreter.LoE
import de.puschj.interpreter.MethodCall
import de.puschj.interpreter.Mul
import de.puschj.interpreter.NEq
import de.puschj.interpreter.Neg
import de.puschj.interpreter.New
import de.puschj.interpreter.Null
import de.puschj.interpreter.Num
import de.puschj.interpreter.Or
import de.puschj.interpreter.Par
import de.puschj.interpreter.Stmt
import de.puschj.interpreter.Sub
import de.puschj.interpreter.VariableProgram
import de.puschj.interpreter.While


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
    
    lazy val start: MultiParser[List[Opt[Stmt]]] = "begin" ~> (repOpt(declaration | statement)) <~ "end"  
   
// =====================
// Statements
// =====================
    
    lazy val statement: MultiParser[Stmt] = expressionStatement | assignStatement | whileStatement | blockStatement | ifStatement | assertStatement
    
    lazy val expressionStatement: MultiParser[Stmt] = expression <~ ";" ^^ {
      case expr => ExprStmt(expr)
    }
    lazy val assignStatement : MultiParser[Assign] = expression ~ "=" ~ expression ~ ";" ^^ {
      case x~_~ e~_ => new Assign(x, e)
    }
    lazy val blockStatement : MultiParser[Block] = "{" ~> repOpt(statement) <~ "}" ^^ { 
      case l => Block(l) 
    }
    lazy val whileStatement : MultiParser[While] = "while" ~> ("(" ~> expression <~ ")") ~ blockStatement ^^ { 
      case c~s => While(c,s) 
    }
    lazy val ifStatement : MultiParser[If] = "if" ~> ("(" ~> expression <~ ")") ~ blockStatement ~ (("else" ~> blockStatement)?) ^^ {
      case c~thn~els => If(c,thn,els)
    } 
    lazy val assertStatement : MultiParser[Assert] = "assert" ~> "(" ~> expression  <~ ")" <~ ";" ^^ {
      case c => Assert(c)
    }
    lazy val declaration: MultiParser[Stmt] = classDeclaration | funcDeclaration
    
    lazy val funcDeclaration : MultiParser[FuncDec] = "def" ~> identifier ~ ("(" ~> repSep(identifier, ",") <~ ")") ~ blockStatement ^^ {
      case funcName~funcArgs~funcBody => FuncDec(funcName, funcArgs, funcBody)
    }
    lazy val fieldDec : MultiParser[Assign] = "var" ~> identifier ~ (("=" ~> expression)?) <~ ";" ^^ {
      case name~Some(expr) => Assign(Id(name), expr)
      case name~None => Assign(Id(name), Null)
    }
    lazy val constDec : MultiParser[Assign] = "const" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
      case name~expr => Assign(Id(name), expr)
    }
    
    lazy val classDeclaration : MultiParser[ClassDec] = "class" ~> identifier ~ (("(" ~> repSep(identifier, ",") <~")")?) ~ (("extends" ~> identifier)?) ~ ("{" ~> 
                                                        repOpt(constDec)) ~ 
                                                        repOpt(fieldDec) ~
                                                        (repOpt(funcDeclaration) <~ "}") ^^ {
      case name~Some(args)~Some(superClass)~consts~fields~funcs => ClassDec(name, args, superClass, consts, fields, funcs)
      case name~Some(args)~None~consts~fields~funcs => ClassDec(name, args, "Object",  consts, fields, funcs)
      case name~None~Some(superClass)~consts~fields~funcs => ClassDec(name, List.empty[Opt[String]], superClass, consts, fields, funcs)
      case name~None~None~consts~fields~funcs => ClassDec(name, List.empty[Opt[String]], "Object",  consts, fields, funcs)
    }

// =====================
// Expressions
// =====================

    lazy val expression: MultiParser[Expr] = cond_3
    
    lazy val cond_3: MultiParser[Expr] = cond_2 ~ repPlain("&&" ~ cond_2 | "||" ~ cond_2) ^^ reduceList
    
    lazy val cond_2: MultiParser[Expr] = cond_1 ~ repPlain("==" ~ cond_1 | "!=" ~ cond_1) ^^ reduceList
    
    lazy val cond_1: MultiParser[Expr] = arith_2 ~ repPlain("<" ~ arith_2 | "<=" ~ arith_2 | ">" ~ arith_2 | ">=" ~ arith_2) ^^ reduceList
      
    lazy val arith_2: MultiParser[Expr] = arith_1 ~ repPlain("+" ~ arith_1 | "-" ~ arith_1) ^^ reduceList
    
    lazy val arith_1: MultiParser[Expr] = negation ~ repPlain("*" ~ negation | "/" ~ negation) ^^ reduceList
    
    lazy val negation: MultiParser[Expr] = (("!")?) ~ access ^^ {
      case None ~ e => e
      case _ ~ e => Neg(e)
    } 
  
    lazy val access: MultiParser[Expr] = factor ~ repPlain("." ~ (call | (identifier ^^ { case s => Id(s) })) ) ^^ reduceList
    
    lazy val factor: MultiParser[Expr] = 
      parenthesis |
      call |
      classNew |
      literal |
      nullExpr |
      integer ^^ { case i => Num(i) } |
      identifier ^^ { case i => Id(i) }
      
    lazy val literal: MultiParser[Expr] = ("false" | "true") ^^ {
      case b => Bool(b.getText.toBoolean)
    }
      
    lazy val call : MultiParser[Call] = identifier ~ ("(" ~> repSep(expression, ",") <~ ")")  ^^ {
      case name~args => Call(name, args)
    }
    
    lazy val classNew : MultiParser[Expr] = "new" ~> identifier ~ ("(" ~> repSep(expression, ",") <~ ")") ^^ {
      case classId~args => New(classId, args)
    }
      
    lazy val parenthesis : MultiParser[Par] = "(" ~> (expression) <~ ")" ^^ {
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
      
      case "." => {
        r._2 match {
          case c: Call => MethodCall(l, c)
          case Id(x) => Field(l, x)
          case e => throw new IllegalArgumentException("Bad syntax.")
        }
      }
    }

    def parse(code:String): VariableProgram = {
      val parser = new WhileParser()
      val y = parser.start(JavaLexer.lex(code), FeatureExprFactory.True)
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