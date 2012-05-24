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
    
    def textToken(t: String): MultiParser[Elem] =
      token(t, _.getText == t)  
      
    def intToken : MultiParser[Elem] =
      token("integer", x=>x.getText().matches("""([1-9][0-9]*)|0"""))
    
    lazy val start: MultiParser[List[Opt[Statement]]] = textToken("begin") ~> repOpt(statement) <~ textToken("end")
  
    // Statement
    lazy val statement: MultiParser[Statement] = assignStatement | whileStatement | blockStatement | ifStatement | assertStatement
    
    lazy val assignStatement : MultiParser[Statement] = identifier ~ textToken("=") ~ expression ~ textToken(";") ^^ {
      case x~_~ e~_ => new Assignment(x.getText(), e)
    }
    lazy val blockStatement : MultiParser[Statement] = textToken("{") ~> repOpt(statement) <~ textToken("}") ^^ { 
      case l => Block(l) 
    }
    lazy val whileStatement : MultiParser[Statement] = textToken("while") ~> (textToken("(") ~> condition <~ textToken(")")) ~ statement ^^ { 
      case c~s => While(c,s) 
    }
    lazy val ifStatement : MultiParser[Statement] = textToken("if") ~> (textToken("(") ~> condition <~ textToken(")")) ~ statement ~ ((textToken("else") ~> statement)?) ^^ {
      case c~thn~els => If(c,thn,els)
    } 
    lazy val assertStatement : MultiParser[Statement] = textToken("assert") ~> textToken("(") ~> condition  <~ textToken(")") <~ textToken(";") ^^ {
      c => Assert(c)
    }

    // Condition
    lazy val equal: MultiParser[Condition] = expression ~ textToken("==") ~ expression ^^ { 
      case e1~_~e2 => Equal(e1,e2)
    }
    lazy val greater: MultiParser[Condition] = expression ~ textToken(">") ~ expression ^^ { 
      case e1~_~e2 => GreaterThan(e1,e2)
    }
    lazy val less: MultiParser[Condition] = expression ~ textToken("<") ~ expression ^^ {
      case e1~_~e2 => LessThan(e1,e2)
    }
    lazy val greaterOrEqual: MultiParser[Condition] = expression ~ textToken(">=") ~ expression ^^ {
      case e1~_~e2 => GreaterOE(e1,e2)
    }
    lazy val lessOrEqual: MultiParser[Condition] = expression ~ textToken("<=") ~ expression ^^ {
      case e1~_~e2 => LessOE(e1,e2)
    }
    lazy val negation = textToken("!") ~> condition ^^ {
      case c => Neg(c)
    }
    lazy val condition : MultiParser[Condition] = equal | greater | less | greaterOrEqual | lessOrEqual

    // Expression
    lazy val factor: MultiParser[Expression] = 
      parenthesis |
      intToken ^^ { x => new Num(Integer.parseInt(x.getText()))} |
      identifier ^^ { x => new Id(x.getText()) }
      
    lazy val parenthesis : MultiParser[Expression] = textToken("(") ~> expression <~ textToken(")") ^^ {
      e => Parens(e)
    }
    lazy val expression: MultiParser[Expression] = term ~ repPlain(textToken("+") ~ term | textToken("-") ~ term) ^^ reduceList
    
    lazy val term: MultiParser[Expression] = factor ~ repPlain(textToken("*") ~ factor | textToken("/") ~ factor) ^^ reduceList
  
    val reduceList: Expression ~ List[Elem ~ Expression] => Expression = {
      case i ~ ps => (i /: ps)(reduce) 
    }
  
    def reduce(l: Expression, r: Elem ~ Expression) = r._1.getText() match {
      case "+" => Add(l, r._2)
      case "-" => Sub(l, r._2)
      case "*" => Mul(l, r._2)
      case "/" => Div(l, r._2)
    }

    def parse(code:String): Program = {
      val parser = new WhileParser()
      val y = parser.start(JavaLexer.lex(code), FeatureExprFactory.True)
      y match{
        case parser.Success(v,_) => {
            val p = new Program(v)
            println(p)
            return p
        }
        case e: parser.NoSuccess =>
            throw new IllegalArgumentException("Bad syntax: "+code)
      }
    }
    
    def parseFile(filename: String): Program = {
      val source = scala.io.Source.fromFile(filename)
      val input = source.mkString
      source.close()
      return parse(input)
    }
}