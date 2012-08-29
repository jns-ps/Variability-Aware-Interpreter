package de.puschj.parser
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers
import de.puschj.interpreter._
import scala.util.parsing.input.CharSequenceReader
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser._
import de.fosd.typechef.conditional._
import de.fosd.typechef.parser.MultiFeatureParser
import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.parser.java15.JavaLexer
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.True

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
    
    lazy val start: MultiParser[List[Opt[Statement]]] = "begin" ~> (repOpt(declaration | statement)) <~ "end"  
   
// =====================
// Statements
// =====================
    
    lazy val statement: MultiParser[Statement] = expressionStatement | assignStatement | whileStatement | blockStatement | ifStatement | assertStatement
    
    lazy val expressionStatement: MultiParser[Statement] = expression <~ ";" ^^ {
      case expr => ExpressionStmt(expr)
    }
    lazy val assignStatement : MultiParser[Assignment] = expression ~ "=" ~ expression ~ ";" ^^ {
      case x~_~ e~_ => new Assignment(x, e)
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
    lazy val declaration: MultiParser[Statement] = classDeclaration | funcDeclaration
    
    lazy val funcDeclaration : MultiParser[FuncDec] = "def" ~> identifier ~ ("(" ~> repSep(identifier, ",") <~ ")") ~ blockStatement ^^ {
      case funcName~funcArgs~funcBody => FuncDec(funcName, funcArgs, funcBody)
    }
    lazy val fieldDec : MultiParser[(String, Expression)] = "var" ~> identifier ~ (("=" ~> expression)?) <~ ";" ^^ {
      case name~Some(expr) => (name, expr)
      case name~None => (name, Null)
    }
    lazy val constDec : MultiParser[(String, Expression)] = "const" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
      case name~expr => (name, expr)
    }
    
    lazy val classDeclaration : MultiParser[ClassDec] = "class" ~> identifier ~ (("(" ~> repSep(identifier, ",") <~")")?) ~ (("extends" ~> identifier)?) ~ ("{" ~> 
                                                        repOpt(constDec)) ~ 
                                                        repOpt(fieldDec) ~
                                                        (repOpt(funcDeclaration) <~ "}") ^^ {
      case name~Some(args)~Some(superClass)~consts~fields~funcs => ClassDec(name, args, superClass, consts, fields, funcs)
      case name~None~None~consts~fields~funcs => ClassDec(name, List.empty[Opt[String]], "Object",  consts, fields, funcs)
      case name~None~Some(superClass)~consts~fields~funcs => ClassDec(name, List.empty[Opt[String]], superClass, consts, fields, funcs)
      case name~Some(args)~None~consts~fields~funcs => ClassDec(name, args, "Object",  consts, fields, funcs)
    }

// =====================
// Expressions
// =====================

    lazy val expression: MultiParser[Expression] = cond_3
    
    lazy val cond_3: MultiParser[Expression] = cond_2 ~ repPlain("&&" ~ cond_2 | "||" ~ cond_2) ^^ reduceList
    
    lazy val cond_2: MultiParser[Expression] = cond_1 ~ repPlain("==" ~ cond_1 | "!=" ~ cond_1) ^^ reduceList
    
    lazy val cond_1: MultiParser[Expression] = arith_2 ~ repPlain("<" ~ arith_2 | "<=" ~ arith_2 | ">" ~ arith_2 | ">=" ~ arith_2) ^^ reduceList
      
    lazy val arith_2: MultiParser[Expression] = arith_1 ~ repPlain("+" ~ arith_1 | "-" ~ arith_1) ^^ reduceList
    
    lazy val arith_1: MultiParser[Expression] = negation ~ repPlain("*" ~ negation | "/" ~ negation) ^^ reduceList
    
    lazy val negation: MultiParser[Expression] = (("!")?) ~ access ^^ {
      case None ~ e => e
      case _ ~ e => Neg(e)
    } 
  
    lazy val access: MultiParser[Expression] = factor ~ repPlain("." ~ (call | (identifier ^^ { case s => Id(s) })) ) ^^ reduceList
    
    lazy val factor: MultiParser[Expression] = 
      parenthesis |
      call |
      classNew |
      literal |
      nullExpr |
      integer ^^ { case i => Num(i) } |
      identifier ^^ { case i => Id(i) }
      
    lazy val literal: MultiParser[Condition] = ("false" | "true") ^^ {
      case b => Bool(b.getText.toBoolean)
    }
      
    lazy val call : MultiParser[Call] = identifier ~ ("(" ~> repSep(expression, ",") <~ ")")  ^^ {
      case name~args => Call(name, args)
    }
    
    lazy val classNew : MultiParser[Expression] = "new" ~> identifier ~ ("(" ~> repSep(expression, ",") <~ ")") ^^ {
      case classId~args => New(classId, args)
    }
      
    lazy val parenthesis : MultiParser[Par] = "(" ~> (expression) <~ ")" ^^ {
      e => Par(e)
    }
    
    val reduceList: Expression ~ List[Elem ~ Expression] => Expression = {
      case i ~ ps => (i /: ps)(reduce) 
    }
  
    def reduce(l: Expression, r: Elem ~ Expression) = r._1.getText() match {
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