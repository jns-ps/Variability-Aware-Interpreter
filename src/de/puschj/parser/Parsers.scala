package de.puschj.parser
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers
import de.puschj.interpreter._
import scala.util.parsing.input.CharSequenceReader



class StatementParser extends ConditionParser {
  
    lazy val prgm : Parser[Program] = (stmt+) ^^ { case s => new Program(s) }
    
    lazy val assignStmt : Parser[Statement] = ident ~ "=" ~ expr ~ ";" ^^ {
      case i~_~e~_ => Assignment(i.toString(), e)
    }
    
    lazy val blockStmt : Parser[Statement] = "{" ~> rep(stmt) <~ "}" ^^ { case l => Block(l) }
    
    lazy val whileStmt : Parser[Statement] = "while" ~> ("(" ~> cond <~ ")") ~ stmt ^^ { case c~s => While(c,s) }
    
    lazy val ifStmt : Parser[Statement] = "if" ~> ("(" ~> cond <~ ")") ~ stmt ~ (("else" ~> stmt)?) ^^ {
      case c~s1~s2 => If(c,s1,s2)
    } 
    
    lazy val stmt = assignStmt | whileStmt | blockStmt | ifStmt
    
    def parse(s: String) = phrase(prgm)(new CharSequenceReader(s))
    
	def apply(s: String): Program = {
		parse(s) match {
		    case Success(tree, _) => tree
		    case e: NoSuccess =>
		           throw new IllegalArgumentException("Bad syntax: "+s)
		}
    }
}


class ConditionParser extends ExpressionParser {
  
  lazy val greater = expr ~ ">" ~ expr ^^ { case e1~_~e2 => GreaterThan(e1,e2) }
  lazy val less = expr ~ "<" ~ expr ^^ { case e1~_~e2 => LessThan(e1,e2) }
  lazy val greaterOrEqual = expr ~ ">=" ~ expr ^^ { case e1~_~e2 => GreaterOrEqualThan(e1,e2) }
  lazy val lessOrEqual = expr ~ "<=" ~ expr ^^ { case e1~_~e2 => LessOrEqualThan(e1,e2) }
  
  lazy val negation = "!" ~ cond ^^ { case _~c => Neg(c) }
  
  lazy val cond : Parser[Condition] = greater | less | greaterOrEqual | lessOrEqual
}



class ExpressionParser extends JavaTokenParsers {

  lazy val int = wholeNumber ^^ { s => Num(s.toInt) }
  lazy val variable = ident ^^ { s => Id(s) }
  lazy val parenthesis : Parser[Expression] = "(" ~> expr <~ ")" ^^ { e => Parens(e) }
  lazy val term = (parenthesis | int | variable)
  lazy val expr = sum | term
  
  lazy val sum = product * ("+" ^^^ { (a:Expression, b:Expression) => Add(a,b) } |
                       "-" ^^^ { (a:Expression, b:Expression) => Sub(a,b) } )
                    
  lazy val product = term * ("*" ^^^ { (a:Expression, b:Expression) => Mul(a,b) } |
		  			    "/" ^^^ { (a:Expression, b:Expression) => Div(a,b) } )

}