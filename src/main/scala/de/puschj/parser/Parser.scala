package de.puschj.parser
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers
import de.puschj.interpreter._
import scala.util.parsing.input.CharSequenceReader
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser._
import de.fosd.typechef.conditional.{One, Choice, Conditional, Opt}
import de.fosd.typechef.parser.MultiFeatureParser
import de.fosd.typechef.parser.java15.TokenWrapper
import de.fosd.typechef.parser.java15.JavaLexer
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory


class WhileParser extends MultiFeatureParser() {
    type Elem = TokenWrapper
    type TypeContext = Null
    
    def identifier: MultiParser[Elem] = token("identifier", x=>x.getText().matches("""[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""))
    
    implicit def textToken(t: String): MultiParser[Elem] =
      token(t, _.getText == t)  
      
    def intToken : MultiParser[Elem] =
      token("integer", x=>x.getText().matches("""([1-9][0-9]*)|0"""))
    
    lazy val start: MultiParser[List[Opt[Statement]]] = "begin" ~> repOpt(statement | funcDeclaration) <~ "end"
  
    // Statement
    lazy val statement: MultiParser[Statement] = assignStatement | whileStatement | blockStatement | ifStatement | assertStatement
    
    lazy val assignStatement : MultiParser[Assignment] = identifier ~ "=" ~ expression ~ ";" ^^ {
      case x~_~ e~_ => new Assignment(x.getText(), e)
    }
    lazy val blockStatement : MultiParser[Block] = "{" ~> repOpt(statement) <~ "}" ^^ { 
      case l => Block(l) 
    }
    lazy val whileStatement : MultiParser[While] = "while" ~> ("(" ~> condition <~ ")") ~ blockStatement ^^ { 
      case c~s => While(c,s) 
    }
    lazy val ifStatement : MultiParser[If] = "if" ~> ("(" ~> condition <~ ")") ~ blockStatement ~ (("else" ~> blockStatement)?) ^^ {
      case c~thn~els => If(c,thn,els)
    } 
    lazy val assertStatement : MultiParser[Assert] = "assert" ~> "(" ~> condition  <~ ")" <~ ";" ^^ {
      case c => Assert(c)
    }
    lazy val funcDeclaration : MultiParser[FuncDef] = "def" ~> identifier ~ ("(" ~> repPlain((identifier ^^ { x => x.getText}) <~ opt(",") ) <~ ")") ~ blockStatement ^^ {
      case funcName~funcArgs~funcBody => FuncDef(funcName.getText, funcArgs, funcBody)
    }

    // Condition
    lazy val equal: MultiParser[Condition] = expression ~ "==" ~ expression ^^ { 
      case e1~_~e2 => Equal(e1,e2)
    }
    lazy val greater: MultiParser[Condition] = expression ~ ">" ~ expression ^^ { 
      case e1~_~e2 => GreaterThan(e1,e2)
    }
    lazy val less: MultiParser[Condition] = expression ~ "<" ~ expression ^^ {
      case e1~_~e2 => LessThan(e1,e2)
    }
    lazy val greaterOrEqual: MultiParser[Condition] = expression ~ ">=" ~ expression ^^ {
      case e1~_~e2 => GreaterOE(e1,e2)
    }
    lazy val lessOrEqual: MultiParser[Condition] = expression ~ "<=" ~ expression ^^ {
      case e1~_~e2 => LessOE(e1,e2)
    }
    lazy val negation = "!" ~> condition ^^ {
      case c => Neg(c)
    }
    lazy val condition : MultiParser[Condition] = equal | greater | less | greaterOrEqual | lessOrEqual

    // Expression
    lazy val call : MultiParser[Expression] = identifier ~ ("(" ~> repPlain(expression <~ opt(",")) <~ ")")  ^^ {
      case name~args => Call(name.getText, args)
    }
    
    lazy val factor: MultiParser[Expression] = 
      parenthesis |
      call |
      intToken ^^ { x => new Num(Integer.parseInt(x.getText()))} |
      identifier ^^ { x => new Id(x.getText()) }
      
    lazy val parenthesis : MultiParser[Expression] = "(" ~> expression <~ ")" ^^ {
      e => Parens(e)
    }
    lazy val expression: MultiParser[Expression] = term ~ repPlain("+" ~ term | "-" ~ term) ^^ reduceList
    
    lazy val term: MultiParser[Expression] = factor ~ repPlain("*" ~ factor | "/" ~ factor) ^^ reduceList
  
    val reduceList: Expression ~ List[Elem ~ Expression] => Expression = {
      case i ~ ps => (i /: ps)(reduce) 
    }
  
    def reduce(l: Expression, r: Elem ~ Expression) = r._1.getText() match {
      case "+" => Add(l, r._2)
      case "-" => Sub(l, r._2)
      case "*" => Mul(l, r._2)
      case "/" => Div(l, r._2)
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