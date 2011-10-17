import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._


trait PyTokens extends Tokens {
  case class Newline(dummy:String) extends Token {
    def chars = dummy
    override def toString = "(NEWLINE)"
  }

  case class Indent(count:Int) extends Token {
    def chars = ""
    var str = ""
    (1 to count) foreach(i=>str = str+"(INDENT)\n")
    override def toString = str.take(str.length-1)
  }

  case class Dedent(count:Int) extends Token {
    def chars = ""
    var str = ""
    (1 to count) foreach(i=>str = str+"(DEDENT)\n")
    override def toString = str.take(str.length-1)
  }

  case class ID(s:String) extends Token {
    def chars = s
    override def toString = "(ID \"" + s + "\")"
  }

  case class Literal(s:String) extends Token {
    def chars = s
//    var str = ""
  //  if(s!=str)
       val str = "(LIT \"" + s + "\")"
    override def toString = str
  }
  
  case class NumLiteral(s:String) extends Token {
    def chars = s
    override def toString = "(LIT " + s + ")"
  }


  case class Keyword(s:String) extends Token {
    def chars = s
    override def toString = "(KEYWORD " + s + ")"
  }

  case class Punctuation(s:String) extends Token {
    def chars = s
    override def toString = "(PUNCT \"" + s + "\")"
  }

  case object Endmarker extends Token {
    def chars = "(ENDMARKER)"
    override def toString = "(ENDMARKER)"
  }


  case class Null(dummy:String) extends Token {
    def chars = "NULLTOKEN"
    override def toString = ""
  }
}

object PySyntax{
		
	//utility functions
	
	def pair_toString(p:Pair[Any,Any]) = {
		"("+p._1+" "+p._2+")"		
	}
	
	def pair_toString1(p:Pair[String,Any]) = {
		"(\""+p._1+"\" "+p._2+")"
	}
	
	def pair_toString2(p:Pair[String,Any]) = {
		if(p._1 .contains("in") || p._1 .contains("not in"))
			"("+p._1+" "+p._2+")"
		else
			"(\""+p._1+"\" "+p._2+")"
	}
	
	def listString(l:List[Any],s:String):String = {
		if(l.size==0) 
			return s.drop(1)
		else
			return listString(l.tail,s+" "+l.head)				
	}
	
	
	abstract trait PyExp
	 
	
	abstract class Factor extends PyExp
	
	case class Factor_unary_op(s:String,f:Factor) extends Factor with PyExp{
		var str = ""
		if(s == null)
			str = f.toString
		else
			str = "(\""+s+"\" "+ f.toString+")"				
		override def toString = str
	}
	
	case class Power(i:Indexed,f:Factor) extends Factor with PyExp{
		var str =""
		if(f==null)
			str = i.toString
		else
			str = "(power "+i.toString+" "+f.toString+")"
				
		override def toString = str
	}
	
	case class Indexed(a:Atom,l:List[Trailer]) extends PyExp{
		var str = ""
		if(l.size == 0)
			str = a.toString
		else
			str = "(indexed "+a.toString+" "+(l mkString " ")+")"		
		override def toString = str
	}
	
	abstract class Trailer extends PyExp
	
	case class Called(a:Arglist) extends Trailer with PyExp{
		var str = ""
		if(a != null)
			str = " "+a.toString
		override def toString = "(called"+str+")"
	}
	
	case class Subscript(t:Tuple_or_test) extends Trailer with PyExp{
		override def toString = "(subscript "+t.toString+")"
	}
	
	case class Dot(s:String) extends Trailer with PyExp{
		override def toString = "(dot "+s+")"
	}
	
	case class testlist(l:List[Test]) extends PyExp {
		override def toString = (l mkString " ")
	}
	
	case class Arglist(l:List[Test]) extends PyExp{
		override def toString = (l mkString " ")
	}
	
	
	
	case class Program(stmts:List[Stmt]) extends PyExp{
		override def toString = "(program "+(stmts mkString " ")+")"
	}
	
	abstract class Stmt extends PyExp
	
	case class Simple_stmt(s:List[Small_stmt]) extends Stmt with PyExp{
		val size = s.size
		var str = ""
		if(size>1)			
			str = "(begin "+(s mkString " ")+")"
		else
			str = s(0).toString
		override def toString = str
	}
	
		
	abstract class Small_stmt extends PyExp
	
	//case class Expr_stmt(a:Aug_assign, test:List[Test], tuple_or_test:Tuple_or_Test) extends Small_stmt{
		
	abstract class Expr_stmt extends Small_stmt with PyExp
	
	case class Aug_assign_expr(a:String,l:List[Test],t:Tuple_or_test) extends Expr_stmt with PyExp{
		override def toString = "(\""+a+"\" ("+(l mkString " ")+")"+t.toString+")"
	}
	
	case class Eq_assign(l:List[Test],t:Tuple_or_test) extends Expr_stmt with PyExp{
		override def toString = "(= ("+(l mkString " ")+")"+t.toString+")"
	}
	
	case class Expr_assign(t:Tuple_or_test) extends Expr_stmt with PyExp{
		override def toString = "(expr "+t.toString+")"
	}
	
	
	case class Del_stmt(s:Star_expr) extends Small_stmt with PyExp{
		override def toString = "(del "+s.toString+")"
	}
	
	
	case class Pass_stmt() extends Small_stmt with PyExp{
		override def toString = "(pass)"
	}
	
	abstract class Flow_stmt extends Small_stmt with PyExp
	
	
	case class Global_stmt(l:List[String]) extends Small_stmt with PyExp{
		override def toString = "(global "+(l mkString " ")+")"
	}
	
	case class Non_local_stmt(l:List[String]) extends Small_stmt with PyExp{
		override def toString = "(nonlocal "+(l mkString " ")+")"
	}
	
	case class Assert_stmt(t1:Test,t2:Test) extends Small_stmt with PyExp{
		override def toString = "(assert "+t1.toString+" "+t2.toString+")"
	}
	
	
	case class Break_stmt() extends Flow_stmt with PyExp{
		override def toString = "(break)"
	}
	
	case class Continue_stmt() extends Flow_stmt with PyExp{
		override def toString = "(continue)"
	}
	
	case class Return_stmt(l:List[Test]) extends Flow_stmt with PyExp{
		override def toString = "(return "+(l mkString " ")+")" 
	}
	
	case class Raise_stmt(t1:Test,t2:Test) extends Flow_stmt with PyExp{
		var t1Str = ""
		var t2Str = ""
		if(t1!=null)
			t1Str = " "+t1.toString
		if(t2!=null)
			t2Str = " "+t2.toString
		override def toString = "(raise"+t1Str+t2Str+")"
	}
	
	
	abstract class Compound_stmt extends Stmt with PyExp
	
	case class Comp_if_stmt(l:List[(Test,Suite)],e:Suite) extends Compound_stmt with PyExp{
		var else_str =""
		if(e!=null)
			else_str = " (else "+e+")"
		override def toString = "(cond "+((l map pair_toString) mkString " ")+else_str+")"
	}
	
	case class Comp_while_stmt(t:Test,s1:Suite,s2:Suite) extends Compound_stmt with PyExp{
		var s2_str = ""
		if(s2 != null)
			s2_str = " "+s2.toString		
		override def toString =  "(while "+t.toString+" "+s1.toString+s2_str+")"
	}
	
	case class Comp_for_stmt(n:String,t:Test,s1:Suite,s2:Suite) extends Compound_stmt with PyExp{
		var tmp = ""
		if(s2!=null)
			tmp = " "+s2.toString
		override def toString = "(for "+n+" "+t.toString+" "+s1.toString+tmp+")"
	}
	
	case class Func_def(l:List[String],s:Suite) extends Compound_stmt with PyExp{
		override def toString = "(def ("+(l mkString " ")+")"+s.toString+")"
	}
	
	case class Comp_try_stmt(s:Suite,l:List[(Catch,Suite)],e:Maybe_else,f:Maybe_finally) extends Compound_stmt with PyExp{
		var newList = "()"
		if(l != null)
			newList = " ("+((l map pair_toString) mkString " ")+")"
		override def toString = "(try "+s.toString+newList+" "+e.toString+" "+f.toString+")"
	}
	
	case class Maybe_else(s:Suite) extends PyExp{
		var str = "#f"
		if(s!=null)
			str = s.toString
		override def toString = str
	}
	
	case class Maybe_finally(s:Suite) extends PyExp{
		var str = "#f"
		if(s!=null)
			str = s.toString
		override def toString = str
	}
	
	case class Catch(t:Test,n:String) extends PyExp{
		var testStr = ""
		var nStr = ""
		if(t != null)
			testStr = " "+t.toString
		if(n != null)
			nStr = " "+n
		override def toString = "(except"+testStr+nStr+")" 
	}
	
	//case class Except()
	
	abstract class Suite extends PyExp
	
	case class Suite_simple(s:Simple_stmt) extends Suite with PyExp{
		override def toString = s.toString
	}
	
	case class Suite_stmt(l:List[Stmt]) extends Suite with PyExp{
		override def toString = "(suite "+(l mkString " ")+")"
	}
	
	
	
	abstract class Test extends PyExp
	
	case class If_expr(p:Or_test,c:Or_test,a:Test) extends Test with PyExp{
		override def toString = "(if "+p.toString+" "+c.toString+" "+a.toString+")"
	}
	
	case class Or_test(l:List[And_test]) extends Test with PyExp{
		var str = ""
		if(l.size>1)			
			str = "(or "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	case class Lambda_def(l:List[String],t:Test) extends Test with PyExp{
		override def toString = "(lambda ("+(l mkString " ")+") "+t+")"
	}
	
	case class And_test(l:List[Not_test]) extends PyExp{
		var str = ""
		if(l.size>1)			
			str = "(and "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	abstract class Not_test extends PyExp
	
	case class Not_not_test(n:Not_test) extends Not_test with PyExp{
		override def toString = "(not "+n.toString+")"
	}
	
	case class Comparison(s:Star_expr, l:List[(String,Star_expr)]) extends Not_test with PyExp{
		var str = ""
		if(l.size == 0)
			str = s.toString
		else{
			str = "(comparison "+s.toString+" "+((l map pair_toString2) mkString " ")+")"
		}
		override def toString = str
	}
	
	/*case class Comparison_star_expr(s:Star_expr) extends Comparison{
		override def toString = "(comparison "+s.toString+")"
	}
	
	case class Compop_star_exp(c:Comp_op, l:List[Star_expr]) extends Comparison{
		var str = ""
		if(l.size>1)			
			str = "("+c.toString+" "+listString(l,"")+")"
		else
			str = l(0).toString
		override def toString = str		
	}*/
	
	case class Star_expr(star:Boolean,l:Expr) extends PyExp{
		var str = ""
		if(!star)
			str = l.toString
		else
			str = "(star "+l.toString+")"
		override def toString = str		
	}
	
	case class Expr(l:List[Xor_expr]) extends PyExp{
		var str = ""
		if(l.size>1)			
			str = "(bitwise-or "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class Xor_expr(l:List[And_expr]) extends PyExp{
		var str = ""
		if(l.size>1)			
			str = "(bitwise-xor "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class And_expr(l:List[Shift_expr]) extends PyExp {
		var str = ""
		if(l.size>1)			
			str = "(bitwise-and "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class Shift_expr(a:Arith_expr,l:List[(String,Arith_expr)]) extends PyExp{
		var str = ""
		if(l.size == 0)
			str = a.toString
		else{
			str = "(shift "+a.toString+" "+((l map pair_toString1) mkString " ")+")"
		}
		override def toString = str
	}
	
	case class Arith_expr(t:Term,l:List[(String,Term)]) extends PyExp{
		var str = ""
		if(l.size == 0)
			str = t.toString
		else
			str = "(arith "+t.toString+" "+((l map pair_toString1) mkString " ")+")"
		override def toString = str
	}
	
	case class Term(f1:Factor,l:List[(String,Factor)]) extends PyExp{
		var str = ""
		if(l.size == 0)
			str = f1.toString
		else
			str = "(term "+f1.toString+" "+((l map pair_toString1) mkString " ")+")"
		override def toString = str
	}
	
	
	
	
	abstract class Atom extends PyExp
	
	case class Atom_string(s:String) extends Atom with PyExp{
		override def toString = s 
	}
	
	case class Atom_tuple_or_test(t:Tuple_or_test) extends Atom with PyExp{
		override def toString = t.toString
	}
	
	case class Atom_testlist (l:Testlist) extends Atom with PyExp{
		var str = ""
		if(l!=null)
			str = " "+l.toString
		override def toString = "(list"+str+")"
	}
	
	case class Atom_dict(d:Dict_or_set) extends Atom with PyExp{
		override def toString = d.toString
	}
	
	abstract class Dict_or_set extends PyExp
	
	case class Dict(l:List[(Test,Test)]) extends Dict_or_set with PyExp{
		override def toString = "(dict "+((l map pair_toString) mkString " ")+")"
	}
	
	case class PSet(l:List[Test]) extends Dict_or_set with PyExp{
		override def toString = "(set "+(l mkString " ")+")"
	}
	
	/*case class Atom_set(l:List[Test]){
		override def toString = "(set "+listString(l,"")+")"
	}*/
	
	case class Tuple_or_test(l:List[Test]) extends Atom with PyExp{
		var str = ""
		if(l.size>1)			
			str = "(tuple "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	case class Testlist(l:List[Test]) extends PyExp{
		override def toString = (l mkString " ")
	}
	
}
    
/*
class PyLexical extends Lexical with PyTokens{
  import scala.util.parsing.input.CharArrayReader.EofCh
  import scala.collection.immutable.HashSet
  import scala.collection.immutable.HashMap
  import scala.collection.mutable.Stack

  var stack : Stack[Int] = new Stack()
  stack.push(0)
  var counter:Int = 0
  
  //type Elem = Char
  //def whitespace : Parser[Any] = rep(whitespaceChar)
  def newline = elem("new line",ch => ch == '\n') | (elem('\r') ~ elem('\n'))
  override def whitespaceChar = elem("space char", ch =>  ch == '\t' || ch == '\r' || ch == '\f')

  def whitespace: Parser[Any] = (
    rep(whitespaceChar)
  )

  def puncts = (elem('^') ~ elem('=')) |(elem('>') ~ elem('>') ~ elem('=')) |(elem('<') ~ elem('<') ~ elem('=')) |(elem('*') ~ elem('*') ~ elem('=')) |(elem('/') ~ elem('/') ~ elem('=')) |(elem('/') ~ elem('=')) |(elem('%') ~ elem('=')) |(elem('&') ~ elem('=')) |(elem('|') ~ elem('=')) |(elem('+') ~ elem('=')) |(elem('-') ~ elem('=')) |(elem('*') ~ elem('=')) | elem("operators", ch => ch == '+' || ch == '-') | (elem('*') ~ elem('*'))|elem('*') | (elem('/') ~ elem('/')) | elem('/') | elem('%') | (elem('<') ~ elem('<')) | (elem('>') ~ elem('>')) | elem('&') | elem('|') | elem('^') | elem('~') | (elem('<') ~ elem('=')) | (elem('<') ~ elem('=')) | (elem('>') ~ elem('=')) | elem('<') | elem('>') | (elem('=')  ~ elem('=')) | (elem('!') ~ elem('=')) | elem("", ch=> ch == ',' || ch == ':' || ch == '.'|| ch == ';' || ch == '@' || ch == '=')

  def add = "+"
  def sub = "-"

  var specialPunct = elem("",ch => ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == '{' || ch == '}')

  def openParenChange(sign:String) = {//val cur:int = counter("(")
                                      if(sign == add)
                                        counter = counter + 1
                                      else
                                        counter = counter - 1
                                    }


  def getSpecialCounter:Boolean = {
	  if(counter>0)
	 	  true
	  else
	  	  false
  }
	  


  def keywords = new HashSet[String] ++ List("False", "class", "finally", "is", "return", "None", "continue", "for", "lambda", "try", "True", "def", "from", "nonlocal", "while", "and", "del", "global", "not", "with", "as", "elif", "if", "or", "yield", "assert", "else", "import", "pass", "break", "except", "in", "raise")

  def id_start = letter | elem('_')
  def charSeq =
      ('\\' ~ '\"' ^^ {case a ~ b => "\\\""}
      |'\\' ~ '\'' ^^ {case a ~ b => "\\\'"}
      |'\\' ~ '\\' ^^ {case a ~ b => "\\\\"}
      |'\\' ~ 't' ^^  {case a ~ b => "\\t"}
      |'\\' ~ '\n'  ^^ {case _ => ""}     
      |'\\' ~ 'n' ^^ {case a ~ b => "\\n"}
      |'\\' ~ '\r' ~ '\n' ^^ {case _ => ""}
      |'\\' ~ elem("",ch=>ch != '\'' || ch != '\"' || ch != EofCh) ^^ {case a ~ b => "\\\\"+b.toString} 
      )

  def squote = elem('\'')
  def dquote = elem('\"')



  def triSquote = squote ~> squote ~> squote 
  def triDquote = dquote ~> dquote ~> dquote

  def stringprefix = opt(elem('r') | elem('R')) ^^ {case None =>  dedentReset ; "" ; case Some(pre) => dedentReset; pre}
  def stringliteral = stringprefix ~> (longstring 
                   |shortstring) 

  def shortstring = (squote ~> rep(shortstringitem) <~ squote ^^ {case b => Literal(b mkString "")} 
                    | dquote ~> rep(shortstringitem) <~ dquote ^^ {case b => Literal(b mkString "")})
  def longstring = (triSquote ~> rep(longstringitem) <~ triSquote ^^ {case d => Literal(d mkString "")}
                    | triDquote ~> rep(longstringitem) <~ triDquote ^^ {case d => Literal(d mkString "")})
  
  def shortstringitem = (elem("",ch=>ch!=EofCh&&ch!='\n'&&ch!='\\'&&ch!='\''&&ch!='\"'&&ch!='\r') | charSeq) 
  def longstringitem = (elem("",ch=>ch!=EofCh&&ch!='\n'&&ch!='\\'&&ch!='\''&&ch!='\"'&&ch!='\r') | charSeq)

  def byteliteral = (elem("",ch => ch =='b' || ch =='B') ~> stringliteral)

  def zero = elem('0')

  def integer =  octinteger | hexinteger | bininteger | decimalinteger
  def decimalinteger =  (nonzerodigit ~ rep(digit) ^^{case a~b => a+(b mkString "")}
                         |rep1(zero) ^^{case a => a mkString ""})
  def nonzerodigit   =  elem("nonzero digit", d => d.isDigit && d != '0')
  def octinteger     =  zero ~ (elem("octal",ch => ch == 'o' | ch == 'O')) ~ rep1(octdigit) ^^ {case a~b~c => "#"+b+(c mkString "")}
  def hexinteger     =  zero ~ (elem("hex", ch => ch == 'x' | ch == 'X')) ~ rep1(hexdigit) ^^ {case a~b~c => "#"+b+(c mkString "")}
  def bininteger     =  zero ~ (elem("binary", ch => ch == 'b' | ch == 'B')) ~ rep1(bindigit) ^^ {case a~b~c => "#"+b+(c mkString "")}
  def octdigit       =  elem("octDigit",d => (d.isDigit && d >= '0' && d<='7'))
  def hexdigit       =  digit | elem("",l => (l.isLetter && l>='a'&& l<='f')) | elem("",l => (l.isLetter && l>='A'&& l<='F'))
  def bindigit       =  zero | elem('1')


    def chr(c:Char) = elem("", ch => ch==c )
    def sign = chr('+') | chr('-')
    def optSign = opt(sign) ^^ {
        case None => ""
        case Some(sign) => sign
    }




def optintpart = opt(rep1(digit)) ^^ {
	case None => ""
	case Some(intpart) => intpart mkString ""
}

def floatnumber = exponentfloat|pointfloat

def pointfloat = ((optintpart ~ fraction) ^^ {case a ~ b => a+b}
		 | (intpart ~ '.') ^^ {case a ~ b => a+b}
		)


def exponentfloat = (intpart ~ exponent ^^ {case a~b => a+b}
 		     |pointfloat ~ exponent ^^ {case a ~ b => a+b}
		    )

def intpart = rep1(digit) ^^ {
	case digits => digits mkString ""
}

def fraction = '.' ~ rep1(digit) ^^ {
	case dot ~ digits => dot + (digits mkString "") 
}

def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
        case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
}

def imaginary = (floatnumber | intpart) ~ elem("",ch => ch=='j' || ch =='J') ^^ {case a~b =>  dedentReset;NumLiteral("+"+a+"i")}


  var end = false

  def comment = elem('#') ~> rep1(elem("",ch => ch != '\n' && ch != EofCh)) <~ optEofCh

  def optEofCh =  opt(elem("",ch => ch== EofCh)) ^^ {
                     case None => ""
                     case Some(a) => end = true}


  def newlineParser:Parser[Token] = (rep1(newline) ~ opt(comment) ~ rep(newline) ^^ {case _ => Null("")}
                               |comment ~ newline ^^ {case _ => Null("")})

  var flag:Boolean = false

  def indentParser(a:String)  ={ if(!flag || getSpecialCounter){
                                   flag = false ; Null("")}
                                 else{
                                   flag = false
                                   val indentLength = a.length
                                   var dedentCount = 0
                                   if(indentLength > stack.top){
                                     stack.push(indentLength)
                                     Indent(1)
                                   }
                                   else if(indentLength < stack.top){
                                     try{
                                     while(indentLength < stack.top){
                                        dedentCount+=1
                                        stack.pop
                                     }
                                     if(indentLength != stack.top)
                                       {println("(Error in indentation)"); exit(1)} 
                                     }catch{case e:Exception => println("incorrect input"); exit(1)}
                                     Dedent(dedentCount)
                                    }
                                    else Null("")
                                   }
                                }
 


  def dedentReset = {if(flag && !getSpecialCounter && stack.top!=0){
                       var dedentCount = 0
                       while(stack.top>0){
                         dedentCount+=1
                         stack.pop
                       }
                       println(Dedent(dedentCount))
                      }
                      flag = false
                    }

  def token: Parser[Token] = expression 

 
  def expression : Parser[Token] = (
    (elem("", ch=>ch == '\"'|| ch == '\'') ~ rep(elem("",ch=>ch!=EofCh&&ch!='\n'&&ch!='\\'&&ch!='\''&&ch!='\"'&&ch!='\r')) ~ (newline|elem("",ch=>ch==EofCh))) ^^ {case a => println("(ERROR \"newline in string\")");exit(1)}
    |rep1(elem(' ')) ~ comment ~ (rep1(newline)) ^^ {case a => 
                                                            if(flag || getSpecialCounter || end)
                                                               Null("")
                                                             else{flag = true
                                                               Newline("")}
                                                  }
    |rep1(elem(' ')) ^^ {case a => indentParser(a mkString "")}

    |imaginary
    |floatnumber ^^ {case a => dedentReset;NumLiteral(a.toString)}
    |integer ^^ {case a => dedentReset;NumLiteral(a.toString)}
    
    |puncts ^^ {case a~b~c => dedentReset;Punctuation(a.toString+b.toString+c.toString); case a ~ b => Punctuation(a.toString + b.toString); case a => Punctuation(a.toString)}
    |specialPunct ^^ {case a => val s = a.toString;dedentReset
                                if(s.contains("(") || s.contains("[") || s.contains("{"))
                                  openParenChange(add)
                                else if(s.contains(")") || s.contains("]") || s.contains("}"))
                                  openParenChange(sub)
                                /*else if(s == "[")
                                  openSquareChange(add)
                                else if(s == "]")
                                  openSquareChange(sub)
                                else if(s == "{")
                                  openFlowerChange(add)
                                else if(s == "}")
                                  openFlowerChange(sub)*/
                                Punctuation(s)
                      }


    |comment ~ opt(rep1(newline)) ^^ {case _ => 
                                     if(flag || getSpecialCounter || end) 
                                       Null("") 
                                     else
                                       Newline("")
                          } 
    |byteliteral
    |stringliteral 
    |(id_start ~ rep(id_start | digit)) ^^ { case first~rest => dedentReset;processIdent(first::rest mkString "") } 
    |EofCh ^^^ Endmarker
    |elem('\\') ~ newline ^^ {case _ => Null("")}
    |rep1(newline) <~ opt(rep1(elem(' '))) <~rep1(newline) ^^ {case _ => flag = true;
									if(getSpecialCounter)
										Null("")
									else
										Newline("")
							      }
    |rep1(newline) <~ opt(comment) <~ rep(newline) ^^ {case _ =>    flag = true;
                                                                    if(getSpecialCounter) 
                                                                       Null("")
                                                                     else 
                                                                       Newline("")
                                                      }
    
    |failure("illegal sequence") 
  )

  def processIdent(name:String) = {
    if(keywords contains name)
      Keyword(name)
    else
      ID(name)
  }


}
*/

class PyLexical extends Lexical with PyTokens{
 import scala.util.parsing.input.CharArrayReader.EofCh
  import scala.collection.immutable.HashSet
  import scala.collection.immutable.HashMap
  import scala.collection.mutable.Stack


  def whitespace : Parser[Any] = rep(whitespaceChar)

  def token : Parser[Token] = elem('(') ~> expression <~elem(')') ^^ {case a => a}

  def expression:Parser[Token] = (newline | indent | dedent | id | keyword | lit | punct | endm) ^^ {case a => a}

  def newline = 'N' ~ 'E' ~ 'W' ~ 'L' ~ 'I' ~ 'N' ~ 'E'^^ {case a => Newline("")}

  def indent = 'I'~'N'~'D'~'E'~'N'~'T' ^^ {case a => Indent(1)}

  def dedent = 'D'~'E'~'D'~'E'~'N'~'T' ^^ {case a => Dedent(1)}

  def id = 'I'~>'D'~>whitespace~>'\"'~> rep1(chrExcept('\"')) <~'\"' ^^ {case a => ID(a mkString "")}

  def lit = strlit | numlit

  def numlit = 'L'~>'I'~>'T'~>whitespace~> integer ^^ {case a => a}

  def integer : Parser[Token] = rep1(chrExcept(')')) ^^{case a => NumLiteral(a mkString "")}

  def strlit = 'L'~>'I'~>'T'~>whitespace~>'\"'~> string <~'\"' ^^ {case a =>a}

    def charSeq =
      ('\\' ~ '\"' ^^ {case a ~ b => "\\\""}
      |'\\' ~ '\'' ^^ {case a ~ b => "\\\'"}
      |'\\' ~ '\\' ^^ {case a ~ b => "\\\\"}
      |'\\' ~ 't' ^^  {case a ~ b => "\\t"}
      |'\\' ~ '\n'  ^^ {case _ => ""}
      |'\\' ~ 'n' ^^ {case a ~ b => "\\n"}
      |'\\' ~ '\r' ~ '\n' ^^ {case _ => ""}
      |'\\' ~ elem("",ch=>ch != '\'' || ch != '\"' || ch != EofCh) ^^ {case a ~ b => "\\"+b.toString}
      )

  def string : Parser[Token] = rep(shortstringitem) ^^ {case b => Literal(b mkString "")}

  def shortstringitem = (elem("",ch=>ch!=EofCh&&ch!='\n'&&ch!='\\'&&ch!='\''&&ch!='\"'&&ch!='\r') | charSeq)

  def keyword = 'K'~>'E'~>'Y'~>'W'~>'O'~>'R'~>'D'~>whitespace~>rep1(letter) ^^ {case a => Keyword(a mkString "")}

  def punct = 'P'~>'U'~>'N'~>'C'~>'T'~>whitespace~>'\"'~>rep1(chrExcept('\"'))<~'\"' ^^ {case a => Punctuation(a mkString "")}

  def endm = 'E'~>'N'~>'D'~>'M'~>'A'~>'R'~>'K'~>'E'~>'R' ^^ {case a => Endmarker} 
}

 
object PyParser extends TokenParsers {
  type Tokens = PyTokens
  override val lexical = new PyLexical
  
  import scala.util.parsing.combinator.lexical.Scanners
  import lexical.{Newline,Indent,Dedent,ID,Literal,NumLiteral,Keyword,Punctuation,Endmarker}

  import PySyntax._
  
   def keyword(chars: String): Parser[String] =
    accept(Keyword(chars)) ^^ (_.chars)
   
     def id_parser(chars: String): Parser[String] =
    	accept(ID(chars)) ^^ (_.chars)
    
    def ident =
    	elem("identifier", _.isInstanceOf[ID]) ^^ (_.chars)
    	
     def punct(s:String): Parser[String] =
    	accept(Punctuation(s)) ^^ (_.chars)
    	
     def numLiteral:Parser[String] =
    	elem("numLiteral", _.isInstanceOf[NumLiteral]) ^^ (_.chars)

    def literal:Parser[String] =
    	elem("literal", _.isInstanceOf[Literal]) ^^ (_.chars)
    	
    def optnewline = opt(rep1(elem("newline",_.isInstanceOf[Newline]))) ^^ {
	  case Some(x) => x
	  case None => ""
    }
  
    def newline_parser = elem("newline",_.isInstanceOf[Newline])
    
    def indent_parser = elem("indent-parser",_.isInstanceOf[Indent])
    
    def dedent_parser = elem("dedent-parser",_.isInstanceOf[Dedent])
    
    def program[Parser[PyExp]] = file_input ^^ {case a => Program(a)}
    
    
    
    def file_input = rep1((newline_parser ^^ {case a => null}) | stmt ^^ {case a =>a}) <~ Endmarker //^^ {case a => a}
    				 
    
    def stmt:Parser[Stmt] = (simple_stmt|compound_stmt) ^^ {case a => a}
    
    def compound_stmt = (if_stmt|while_stmt|for_stmt|try_stmt|funcdef) ^^ {case a => a}
    
    def else_suite = keyword("else") ~> punct(":") ~> suite ^^ {
    	case a => a
    }
    
    def opt_else_suite = opt(else_suite) ^^ {
    	case Some(a) => a
    	case None => null
    }
    
    def suite = simple_stmt ^^ {case a => Suite_simple(a)} | 
    			(newline_parser ~ indent_parser ~ rep1(stmt) ~ dedent_parser) ^^ {
    				case a~b~c~d => Suite_stmt(c)
    			}
    
    def elif_test_suite = (keyword("elif") ~ test ~ punct(":") ~ suite) ^^ {
    	case a~b~c~d => (b,d)
    }
    
    def if_stmt = keyword("if") ~ test ~ punct(":") ~ suite ~ rep(elif_test_suite) ~ opt_else_suite ^^{
    	case a~b~c~d~e~f => Comp_if_stmt(((b,d)::e),f)
    }
    
    def while_stmt = keyword("while") ~ test ~ punct(":") ~ suite ~ opt_else_suite ^^ {
    	case a~b~c~d~e => Comp_while_stmt(b,d,e)
    }
    
    def for_stmt = keyword("for") ~ ident ~ keyword("in") ~ test ~ punct(":") ~ suite ~ opt_else_suite ^^ {
    	case a~b~c~d~e~f~g => Comp_for_stmt(b,d,f,g)
    }
    
    
    def catch_colon_suite = except_clause ~ punct(":") ~ suite ^^ {
    	case a ~ b ~ c => (a,c)
    }
    
    def opt_finally_suite = opt(finally_suite) ^^{
    	case Some(a) => a
    	case None => null
    }
    def finally_suite = (keyword("finally") ~> punct(":") ~> suite) ^^ {
    	case a => a
    }
    
    def catch_else_finally = (rep1(catch_colon_suite) ~ opt_else_suite ~ opt_finally_suite) ^^{
    	case a~b~c => (a,Maybe_else(b),Maybe_finally(c))
    }
    
    def try_finally = finally_suite ^^ {
    	case a => (null,Maybe_else(null),Maybe_finally(a))
    }
    
    def catch_else_or_finally = catch_else_finally | try_finally ^^ {case a => a}
    	
    def try_stmt = keyword("try") ~ punct(":") ~ suite ~ catch_else_or_finally ^^ {
    	case a~b~c~d => Comp_try_stmt(c,d._1,d._2,d._3)
    }
    						
    def opt_as_name = opt(keyword("as") ~> ident) ^^{
    	case Some(a) => a
    	case None => null
    }
    
    def test_opt_as_name = opt(test ~ opt_as_name) ^^{
    	case Some(a~b) => (a,b)
    	case None => null
    }
    						
    def except_clause = keyword("except") ~> test_opt_as_name ^^ {
    	case a => if(a!=null)
    				Catch(a._1,a._2)
    			  else
    			 	Catch(null,null)  
    }
    
    def funcdef = keyword("def") ~ ident ~ parameters ~ punct(":") ~ suite ^^{
    	case a~b~c~d~e => Func_def(b::c,e)
    }
    
    def parameters = punct("(") ~> opt_paramlist <~ punct(")") ^^ {
    	case a => val xx = a
    			  a
    }
    
    def optsemicolon = opt(punct(";")) ^^ {
    	case Some(x) => x
    	case None => ""
    }
    
    def simple_stmt = small_stmt ~ rep(punct(";") ~> small_stmt) ~ optsemicolon ~ newline_parser ^^ {
    	case a~b~c~d => Simple_stmt(a::b)  	
    }
        
    
    def small_stmt:Parser[Small_stmt] = expr_stmt ^^ {case a => a}|del_stmt ^^ {case a => a}|pass_stmt ^^ {case a => a}|
    		flow_stmt ^^ {case a => a}|global_stmt^^ {case a => a}|nonlocal_stmt ^^ {case a => a} |
    		assert_stmt^^ {case a => a}
    
    def pass_stmt = keyword("pass") ^^ {case e => Pass_stmt()}
    
    //def expr_stmt = augassign ~ rep1(test) ~ tuple_or_test ^^ {case a~b~c => Aug_assign_expr(a,) | 
    	//			"=" ~ rep1(test) ~ tuple_or_test
    def tuple_or_test = test ~ rep(punct(",") ~> test) <~ opt(punct(",")) ^^ {case a ~ b => Tuple_or_test(a::b)}
    
    def expr_stmt:Parser[Expr_stmt] = (testlist ~ augassign ~ tuple_or_test) ^^ {case a~b~c => Aug_assign_expr(b,a.l,c)} |
    				(testlist ~ punct("=") ~ tuple_or_test) ^^ {case a~b~c => Eq_assign(a.l,c)} |
    				(tuple_or_test) ^^ {case a => Expr_assign(a)}
    
    
    
    
    def augassign = (punct("+=")| punct("-=") | punct("*=") | punct("/=")  | punct("%=")| punct("&=") | 
    		punct("|=") | punct("^=") | punct("<<=") | punct(">>=") | punct("**=") | punct("//=")) ^^ {
    	case a => a
    }
    def test:Parser[Test] = (or_test ~ keyword("if") ~ or_test ~ keyword("else") ~ test) ^^ {case a~b~c~d~e => If_expr(a,c,e)} | 
    	or_test ^^ {case a => a} | 
    	lambdadef ^^ {case a =>a}
    
    def opt_paramlist = opt(paramlist) ^^ {
    	case Some(a) => a
    	case None => List()
    }
    
    def opt_punct_comma = opt(punct(",")) ^^{
    	case Some(z) => z
    	case None => null
    }
    	
    def lambdadef = keyword("lambda") ~ opt_paramlist ~ punct(":") ~ test ^^ { case a~b~c~d => Lambda_def(b,d)}
    
    def paramlist:Parser[List[String]] = ident ~ rep(punct(",") ~> ident) <~ opt(punct(",")) ^^ {case a ~ b => a::b}
    																				 
    																				
    def or_test = and_test ~ rep(keyword("or") ~> and_test) ^^ {case a~b => Or_test(a::b)}
    def and_test = not_test ~ rep(keyword("and") ~> not_test) ^^ {case a~b => And_test(a::b)}
    def not_test:Parser[Not_test] = comparison ^^ {case a => a}| 
    								keyword("not") ~> not_test ^^ {case a => Not_not_test(a)}
    								
    def comp_op_star_expr = comp_op ~ star_expr ^^ {case a ~ b => (a,b)}
    def comparison = star_expr ~ rep(comp_op_star_expr) ^^ {case a ~ b => Comparison(a,b)}
    
    def comp_op:Parser[String] =  punct("<") | punct(">") | punct("==") | punct(">=") | punct("<=") | id_parser("<>") | 
    						  punct("!=") | keyword("in")| keyword("not") ~ keyword("in") ^^ {case a~b => a+" "+b} | 
    						  keyword("is") | keyword("is") ~ keyword("not") ^^ {case a~b => a+" "+b}

    def opt_star = opt(punct("*")) ^^ {
    	case Some(a) => true
    	case None => false
    }
    def star_expr = (opt_star ~ expr) ^^ {case a ~ b => Star_expr(a,b)} 
    def expr = xor_expr ~ rep(punct("|") ~> xor_expr) ^^ {case a ~ b => Expr(a::b)}
    def xor_expr = and_expr ~ rep(punct("^") ~> and_expr) ^^ {case a ~ b => Xor_expr(a::b)}
    def and_expr = shift_expr ~ rep(punct("&") ~> shift_expr) ^^ {case a ~ b => And_expr(a::b)}
    
    def punct_arith_expr = (punct(">>")|punct("<<")) ~ arith_expr ^^ {
    	case a~b => (a,b)
    }
    def shift_expr = arith_expr ~ rep(punct_arith_expr) ^^ {case a ~ b => Shift_expr(a,b)}
    
    def punct_term = (punct("+")|punct("-")) ~ term ^^ {
    	case a ~ b => (a,b)
    }
    
    def arith_expr = term ~ rep(punct_term) ^^ {case a~b => Arith_expr(a,b)}
    
    def op_factor = ((punct("*")|punct("/")|punct("%")|punct("//")) ~ factor) ^^ {
    	case a ~ b => (a,b)
    }
    def term = factor ~ rep(op_factor) ^^ {case a~b => Term(a,b)}
    def factor:Parser[Factor] = ((punct("+") | punct("-") | punct("~")) ~ factor) ^^ {case a ~ b => Factor_unary_op(a,b)}|
    							power ^^ {case a => a}
    
    def opt_star_factor = opt(punct("**") ~> factor) ^^ {
    	case Some(a) => a
    	case None => null
    }
    def power = indexed ~ opt_star_factor ^^ {case a ~ b => Power(a,b)}
    def indexed = atom ~ rep(trailer) ^^ {case a ~ b => Indexed(a,b)}
    
    
    def trailer = (punct("(") ~> opt(arglist) <~ punct(")")) ^^ {case Some(a) => Called(a)
    											   case None => Called(null) 
    												} |
    			  (punct("[") ~> tuple_or_test <~ punct("]")) ^^ {case a => Subscript(a)} |
    			  (punct(".") ~> ident) ^^ {case a => Dot(a)}
    			  
    def arglist = test ~ rep(punct(",") ~> test) <~ opt(punct(",")) ^^ {case a~b => Arglist(a::b)}
    
    /*def opt_tuple_or_test = opt(tuple_or_test) ^^ {
    	case Some(a) => a
    	case None => Tuple_or_test(List())
    }*/
    
    def opt_tuple_or_test: Parser[Atom] = opt(tuple_or_test) ^^{
    	case Some(a) => Atom_tuple_or_test(a)
    	case None => null
    }
    
    def opt_testlist = opt(testlist) ^^ {
    	case Some(a) => Atom_testlist(a)
    	case None => null
    }
    
    def opt_dictorsetmaker: Parser[Atom] = opt(dictorsetmaker) ^^ {
    	case Some(a) => Atom_dict(a)    		
    	case None => null
    }
    
    def test_punct_test = (test ~ punct(":") ~ test) ^^ {
    	case a~b~c => (a,c)
    }
    def dictorsetmaker = ((test_punct_test ~ rep(punct(",") ~> test_punct_test)) <~ opt(punct(","))) ^^{
    							case a~b => Dict(a::b)
    						} |
    					((test ~ rep(punct(",") ~> test)) <~ opt(punct(","))) ^^ {
    							case a~b => PSet(a::b)
    						}
    
    def atom:Parser[Atom] = (punct("(") ~> opt_tuple_or_test <~ punct(")")) ^^ {case a => a}  | 
    		(punct("[") ~> opt_testlist <~ punct("]")) ^^ {case a => a} | 
    		(punct("{") ~> opt_dictorsetmaker <~ punct("}")) ^^ {case a =>a} |
    		(ident | numLiteral) ^^ {case a => Atom_string(a.toString)} |
    		literal ^^ {case a =>Atom_string("\""+a.toString.toString+"\"")}|
                id_parser("...") ^^ {case a => Atom_string("...")} | 
                Keyword("None") ^^ {case a => Atom_string("None")} | 
                Keyword("True") ^^ {case a => Atom_string("True")} | 
                Keyword("False") ^^ {case a => Atom_string("False")}|  
                failure("failed in atom")
     
    
    def opt_test = opt(punct(",") ~> test) ^^ {
    	case Some(a) => a
    	case None => null
    }
    
    def opt_from_test = opt(keyword("from") ~> test) ^^ {
    	case Some(a) => a
    	case None => null    	
    }
    
    def opt_test_from_test = opt(test ~ opt_from_test) ^^ {
    	case Some(a~b) => (a,b)
    	case None => null
    }
            
    
    
    
    def del_stmt = keyword("del") ~ star_expr ^^ {case a~b => Del_stmt(b)}
    
    def global_stmt = keyword("global") ~ ident ~ rep(punct(",") ~> ident) ^^ {case a~b~c => Global_stmt(b::c)}
    def nonlocal_stmt = keyword("nonlocal") ~ ident ~ rep(punct(",") ~> ident) ^^ {case a~b~c => Non_local_stmt(b::c)}
    def assert_stmt = keyword("assert") ~ test ~ opt_test ^^ {case a~b~c => Assert_stmt(b,c)}
    def break_stmt:Parser[Flow_stmt] = keyword("break") ^^ {case _ => Break_stmt()}
    def continue_stmt: Parser[Flow_stmt] = keyword("continue") ^^ {case _ => Continue_stmt()}
    def return_stmt:Parser[Flow_stmt] = keyword("return") ~ opt_testlist ^^ {case a ~ b => Return_stmt(b.l.l)}
    def raise_stmt:Parser[Flow_stmt] = keyword("raise") ~ opt_test_from_test ^^ {case a~b => Raise_stmt(b._1,b._2)}
    
    def testlist = (test ~ rep(punct(",") ~> test)) <~ opt(punct(",")) ^^ {case a~b => Testlist(a::b)}
    	
    def flow_stmt = break_stmt ^^ {case a => a}|continue_stmt ^^ {case a => a}|return_stmt^^ {case a => a}|
    		raise_stmt^^ {case a => a}
    
    def dummy = ident ~ punct(")") //~ parameters ~ punct(":") ~ suite ^^{
    	//case a~b~c~d~e => Func_def(b::c,e)
    //}
    
    import scala.util.parsing.input._
    def filter[T](reader: Reader[T], check: T => Boolean): Reader[T] = {
    new Reader[T] {
      var orig = reader
      def first = { trim; orig.first }
      def atEnd = { trim; orig.atEnd }
      def rest: Reader[T] = { trim; filter(orig.rest, check) }
      def pos = orig.pos
      private def trim = {
        while (!orig.atEnd && !check(orig.first))
          orig = orig.rest
      }
     }
    } 
    
    
    
    
    
    def parse(s:String) = { 
    	program (filter(new lexical.Scanner(s),{t:PyParser.lexical.Token => t.chars != "NULLTOKEN"})) match {
    		case Success(mx, _) => mx
 
    		case Failure(msg,_) =>
    			throw new Exception("parse failed: " + msg)

    		case Error(msg,_) =>
    			throw new Exception("parse error: " + msg)
    	}
    }
  
  def print(s:String) = {
        val tokens:lexical.Scanner = new lexical.Scanner(s)
        var tk:lexical.Scanner = tokens
       /* while(tk.first.toString == "(NEWLINE)" || tk.first.toString == "")
          tk = tk.rest
        if(tk.first.toString == "(INDENT)")
          {println("(Error - first line indented)"); exit(1)}*/
	def printer(sc:lexical.Scanner):Unit = {
          if(!sc.atEnd){
            if(sc.first.toString != "")
              println(sc.first)
            printer(sc.rest)
          }
        }
        printer(tk)
  }
  
  
}

object Hir_transformer {
	import PySyntax._
	
	var global_var_list:List[String] = List()
	
	abstract trait HirExp
	
	case class my_stmts_list(l:List[exp]) extends exp with HirExp{
		override def toString = l mkString " " 
	}
	
	case class program(l:List[top_form]) extends HirExp{
		var str:List[String] = List()
		if(global_var_list.size > 0)
			str = global_var_list.map((x) => "(define "+x.toString+" (void))")
		override def toString = "(program "+
									(str mkString " ") +
									(l mkString " ")+")"		
	}
	
	abstract class top_form extends HirExp
	
	case class vardef(v:var_exp,e:exp) extends top_form with HirExp{
		override def toString = "(define "+v.toString+" "+e.toString+")"
		
	}
	
	abstract class exp extends top_form with HirExp
	
	case class void_exp extends exp with HirExp{
		override def toString = "(VOID)"
	}
	
	case class error_exp(e:exp) extends exp with HirExp{
		override def toString = "(error "+e.toString+")"
	}
	
	case class lambda_exp(l:List[var_exp],e:exp) extends exp with HirExp{
		override def toString = "(lambda ("+(l mkString " ")+") "+e.toString+")"
	}
	
	case class call_ec_exp(l:lambda_exp) extends exp with HirExp{
		override def toString = "(call/ec "+l.toString+")"
	}
	
	case class var_exp(s:String) extends exp with HirExp{
		override def toString = s
	}
	
	case class num_exp(i:Int) extends exp with HirExp{
		override def toString = i.toString
	}
	
	case class string_exp(s:String) extends exp with HirExp{
		override def toString = s
	} 
	
	case class integer_huh extends exp with HirExp{
		override def toString = "integer?"
	}
	
	case class string_huh extends exp with HirExp{
		override def toString = "string?"
	}
	
	case class tuple_huh extends exp with HirExp{
		override def toString = "tuple?"
	}
	
	case class dict_huh extends exp with HirExp{
		override def toString = "dict?"
	}
	
	case class pylist_huh extends exp with HirExp{
		override def toString = "py-list?"
	}
	
	case class set_huh extends exp with HirExp{
		override def toString = "set?" 
	}
	
	
	
	case class set_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(set "+(l mkString " ")+")"
	}
	
	case class py_list(l:List[exp]) extends exp with HirExp{
		override def toString = "(pylist* "+(l mkString " ")+")"
	}
	
	case class dict_exp(l:List[(exp,exp)]) extends exp with HirExp{
		override def toString = "(dict "+((l map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+")"
	}
	
	case class tuple_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(tuple "+(l mkString " ")+")"
	}
	
	case class let_exp(binds:List[(var_exp,exp)], l:List[exp]) extends exp with HirExp{
		override def toString = "(let ("+((binds map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+
								") "+(l mkString " ")+")"
	}
	
	case class set_bang_exp(v:var_exp,e:exp) extends exp with HirExp{
		override def toString = "(set! "+v.toString+" "+e.toString+")"
	}
	
	case class py_list_ref(e1:exp, e2:exp) extends exp with HirExp{
		override def toString = "(py-list-ref "+e1.toString+" "+e2.toString+")"
	}
	
	case class py_list_set_bang(e1:exp, e2:exp, e3:exp) extends exp with HirExp{
		override def toString = "(py-list-set! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class py_list_remove_bang(e1:exp, e2:exp) extends exp with HirExp{
		override def toString = "(py-list-remove! "+e1.toString+" "+e2.toString+")"
	}
	
	case class get_field(e1:exp, e2:var_exp) extends exp with HirExp{
		override def toString = "(get-field "+e1.toString+" "+e2.toString+")"
	}
	
	case class set_field_bang(e1:exp, e2:var_exp, e3:exp) extends exp with HirExp{
		override def toString = "(set-field! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class remove_field_bang(e1:exp, e2:var_exp) extends exp with HirExp{
		override def toString = "(remove-field! "+e1.toString+" "+e2.toString+")"
	}
	
	case class get_global(v:var_exp) extends exp with HirExp{
		override def toString = "(get-global "+v.toString+")"
	}
	
	case class set_global_bang(v:var_exp,e:exp) extends exp with HirExp{
		override def toString = "(set-global! "+v.toString+" "+e.toString+")"
	}
	
	case class throw_exp(e:exp) extends exp with HirExp{
		override def toString = "(throw "+e.toString+")"
	}
	
	case class try_exp(e1:exp,e2:exp) extends exp with HirExp{
		override def toString = "(try "+e1.toString+" "+e2.toString+")"
	}
	
	case class assert_exp(e1:exp,e2:exp) extends exp with HirExp{
		var str = ""
		if(e2!=null)
			str = " "+e2.toString
		override def toString = "(assert "+e1.toString+str+")"
	}
	
	case class cond_exp(conds:List[(exp,exp)], els:exp) extends exp with HirExp{
		var str = ""
		if(els!=null)
			str = " (else "+els.toString+")"
		override def toString = "(cond "+((conds map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+
							str+")"
	}
	
	case class if_exp(a:exp,b:exp,c:exp) extends exp with HirExp{
		override def toString = "(if "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class and_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(and "+(l mkString " ")+")"
	}
	
	case class or_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(or "+(l mkString " ")+")"
	}
	
	case class not_exp(e:exp) extends exp with HirExp{
		override def toString = "(not "+e.toString+")"
	}
	
	case class while_exp(a:exp,b:exp,c:exp) extends exp with HirExp{
		var str = ""
		if(c!=null)
			str = " "+c.toString
		override def toString = "(while "+a.toString+" "+b.toString+str+")"
	}
	
	case class for_each_exp(v:var_exp,a:exp,b:exp,c:exp) extends exp with HirExp{
		var str = ""
		if(c!=null)
			str = " "+c.toString
		override def toString = "(for_each "+v.toString+" "+a.toString+" "+b.toString+str+")"
	}
	
	case class break_exp extends exp with HirExp{
		override def toString = "(break)"
	}
	
	case class continue_exp extends exp with HirExp{
		override def toString = "(continue)"
	}
	
	case class my_return_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(return "+(l mkString " ")+")"
	}
	
	case class begin_exp(l:List[exp]) extends exp with HirExp{
		override def toString = "(begin "+(l mkString " ")+")"
	}
	
	case class multop_exp(m:multop,l:List[exp]) extends exp with HirExp{
		override def toString = "("+m.toString+" "+(l mkString " ")+")"
	}
	
	case class binop_exp(b:binop,e1:exp,e2:exp) extends exp with HirExp{
		override def toString = "("+b.toString+" "+e1.toString+" "+e2.toString+")" 
	}
	
	case class unop_exp(u:unop,e:exp) extends exp with HirExp{
		override def toString = "("+u.toString+" "+e.toString+")" 
	}
	
	case class multop(op:String) extends HirExp{
		override def toString = op
	}
	
	case class binop(op:String) extends HirExp{
		override def toString = op
	}
	
	case class unop(op:String) extends HirExp{
		override def toString = op
	}
	
	case class py_print(e:exp) extends exp with HirExp{
		override def toString = "(py-print "+e.toString+")"
	}
	
	def hir_transform(exp:PyExp):HirExp = {
		exp match {
			case null => null
			
			case e:Factor_unary_op => unop_exp(unop(e.s),hir_transform(e.f).asInstanceOf[exp])
			
			case e:Power => hir_transform(e.i)
			
			/*this indexed is for its appearance on the r-value of an expression, which means just reference*/
			
			case e:Indexed => {
				if(e.l.size == 0)
					hir_transform(e.a)
				else{
					e.a match{
						case as:Atom_string => if(as.s.contains("print"))
												return py_print(my_stmts_list((e.l map ((x) => hir_transform(x).asInstanceOf[exp]))))
						case _ => 
					}
					hir_transform(e.a)
				}					
			}
			/*{
				val atom_bind = gensym()
				val first_trailer_bind = gensym()
				val outer_let_binds = (var_exp(atom_bind), hir_transform(e.a).asInstanceOf[exp])
				val inner_let_binds = e.l(0) match {
					case s:Subscript => (var_exp(first_trailer_bind), (hir_transform(s.t).asInstanceOf[exp]))
					case c:Called => c.a match {
						case 
					}
				}
				val inner_let_body = cond_exp()
				let_exp(List(outer_let_binds),  )
			}
			*/
			case e:Called => hir_transform(e.a)
			 
		/*	case e:Subscript => e
			
			case e:Dot => e
			*/ 
			case e:testlist => py_list(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			
			case e:Arglist => py_list(e.l map ((x) => hir_transform(x).asInstanceOf[exp])) 
			
			case e:Program => program(e.stmts map ((stmt) => 
													hir_transform(stmt) match{
														case a:top_form => a.asInstanceOf[top_form]
														case _ => throw new Exception("failed in hir translation of Program")
													}
													))
													
			
			
			case e:Simple_stmt => {
				if(e.s.size == 1)
					hir_transform(e.s(0))
				else
					begin_exp(e.s map ((x) => hir_transform(x) match{ 			
																case a:exp => a.asInstanceOf[exp]
																case _ => throw new Exception("failed in hir translation of simple_stmt")
															}))
			}
			
											
	/*		 
			case e:Aug_assign_expr => {
				val test_plus = e.l map ((x)=>hir_transform(x).asInstanceOf[exp])
				val tuple_or_test = hir_transform(e.t).asInstanceOf[exp]
				
				for(i <- 0 to test_plus.size){
					i match{
						case v:var_exp => plist = set_bang_exp(v.asInstanceOf[var_exp],tuple_or_test(i))::plist 
					}
					
				}
				e.a match {
					case "&=" => multop_exp(multop("&"), e.l map ((x)=>hir_transform(x).asInstanceOf[exp]))
					case "|=" => multop_exp(multop("&"), e.l map ((x)=>hir_transform(x).asInstanceOf[exp]))
					case "^=" => multop_exp(multop("&"), e.l map ((x)=>hir_transform(x).asInstanceOf[exp]))
				}				
			}
			
			case e:Eq_assign => e
	*/
			case e:Expr_assign => hir_transform(e.t)
	/*
			case e:Del_stmt => e
			
			case e:Pass_stmt => e
	
			case e:Global_stmt => e
	
			case e:Non_local_stmt => e
			*/
			case e:Assert_stmt => assert_exp(hir_transform(e.t1).asInstanceOf[exp],hir_transform(e.t2).asInstanceOf[exp])
			
			case e:Break_stmt => break_exp()
			
			case e:Continue_stmt => continue_exp()
			
			case e:Return_stmt => {
				
					let_exp(List(), List(my_return_exp(e.l map ((x) => hir_transform(x).asInstanceOf[exp])).asInstanceOf[exp]))
			}
			
			case e:Raise_stmt => throw_exp(hir_transform(e.t1).asInstanceOf[exp])
	
			case ex:Comp_if_stmt => {
				val conds = (ex.l map ((x) => (hir_transform(x._1).asInstanceOf[exp],hir_transform(x._2).asInstanceOf[exp])))
				val else_ = hir_transform(ex.e).asInstanceOf[exp]
				cond_exp(conds, else_)										
			}
			
			case e:Comp_while_stmt => while_exp(hir_transform(e.t).asInstanceOf[exp],hir_transform(e.s1).asInstanceOf[exp], hir_transform(e.s2).asInstanceOf[exp])
	
			case e:Comp_for_stmt => for_each_exp(var_exp(e.n), 
											hir_transform(e.t).asInstanceOf[exp],
											hir_transform(e.s1).asInstanceOf[exp],
											hir_transform(e.s2).asInstanceOf[exp])
	
			case e:Func_def => {val expression = hir_transform(e.s) match{
													case a:exp => a.asInstanceOf[exp]
													case _ => throw new Exception("failed in hir translation of func_def")
												}
								global_var_list = e.l(0)::global_var_list
								val lambda_body = call_ec_exp(lambda_exp(List(var_exp("return")),let_exp(List(),List(expression))))
								set_global_bang(var_exp(e.l(0)),			
										lambda_exp(e.l.drop(1) map ((x) => var_exp(x)),
												lambda_body))
			}
			
			case e:Comp_try_stmt => {
				try_exp(hir_transform(e.s).asInstanceOf[exp],hir_transform(e.l(0)._2).asInstanceOf[exp])
			}
	
			
			
			case e:Suite_simple => hir_transform(e.s)
			
			case e:Suite_stmt => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					my_stmts_list(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			}
			
			case e:If_expr => if_exp(hir_transform(e.p).asInstanceOf[exp], 
									hir_transform(e.c).asInstanceOf[exp], 
									hir_transform(e.a).asInstanceOf[exp])
			
			case e:Or_test =>{ 
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					or_exp(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			}
			
			case e:Lambda_def => lambda_exp(e.l map var_exp, hir_transform(e.t).asInstanceOf[exp])
			
			case e:And_test => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					and_exp(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			}
			
			case e:Not_not_test => not_exp(hir_transform(e.n).asInstanceOf[exp])
			
			case e:Comparison => {
				if(e.l.size == 0)
					hir_transform(e.s) 
				else{
					val s = hir_transform(e.s).asInstanceOf[exp]
					val first_comp_op_star_expr = e.l(0)
					var prev_exp = binop_exp(binop(first_comp_op_star_expr._1),s,hir_transform(first_comp_op_star_expr._2).asInstanceOf[exp])
					val new_list = e.l.drop(1)
					for(i<-new_list){
						prev_exp = binop_exp(binop(i._1),prev_exp,hir_transform(i._2).asInstanceOf[exp])
					}
					prev_exp 
				}
			}
	
			case e:Star_expr =>	hir_transform(e.l)
				
			
			case e:Expr => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					multop_exp(multop("|"),(e.l map ((x) => hir_transform(x).asInstanceOf[exp])))
			}
	
			case e:Xor_expr => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					multop_exp(multop("^"),(e.l map ((x) => hir_transform(x).asInstanceOf[exp])))
			}
			
			case e:And_expr => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					multop_exp(multop("&"),(e.l map ((x) => hir_transform(x).asInstanceOf[exp])))					
			}
			
			case e:Shift_expr => {
				if(e.l.size == 0)
					hir_transform(e.a) 
				else{
					val a = hir_transform(e.a).asInstanceOf[exp]
					val first_shiftop_arith_expr = e.l(0)
					var prev_exp = binop_exp(binop(first_shiftop_arith_expr._1),a,hir_transform(first_shiftop_arith_expr._2).asInstanceOf[exp])
					val new_list = e.l.drop(1)
					for(i<-new_list){
						prev_exp = binop_exp(binop(i._1),prev_exp,hir_transform(i._2).asInstanceOf[exp])
					}
					prev_exp 
				}
			}
			
			case e:Arith_expr => {
				if(e.l.size == 0)
					hir_transform(e.t)
				else{
					val t = hir_transform(e.t).asInstanceOf[exp]
					val first_arithop_term = e.l(0)
					var prev_exp = binop_exp(binop(first_arithop_term._1),t,hir_transform(first_arithop_term._2).asInstanceOf[exp])
					val new_list = e.l.drop(1)
					for(i<-new_list){
						prev_exp = binop_exp(binop(i._1),prev_exp,hir_transform(i._2).asInstanceOf[exp])
					}
					prev_exp
				}
			}
	
			case e:Term => {if(e.l.size==0)
								hir_transform(e.f1)
							else{
								val f1 = hir_transform(e.f1).asInstanceOf[exp]
								val first_binop_factor = e.l(0)
								var prev_exp = binop_exp(binop(first_binop_factor._1),f1,hir_transform(first_binop_factor._2).asInstanceOf[exp])
								val new_list = e.l .drop(1)
								for(i<-new_list){
									prev_exp = binop_exp(binop(i._1),prev_exp,hir_transform(i._2).asInstanceOf[exp])
								}
								prev_exp
							}
								
			}
			
			case e:Atom_string => string_exp(e.s)
	
			case e:Atom_tuple_or_test => hir_transform(e.t)
			
			case e:Atom_testlist => hir_transform(e.l)
	
			case e:Atom_dict => {
				e match {
					case d:Dict => {
						val dbody:List[(exp,exp)] = d.l map ((x) => 
															(hir_transform(x._1).asInstanceOf[exp],hir_transform(x._2).asInstanceOf[exp]))		
						dict_exp(dbody)
					}
					case s:PSet => set_exp(s.l map ((x) => hir_transform(x).asInstanceOf[exp]))
				}
			}
			
			case e:Dict => dict_exp(e.l map ((x) =>(hir_transform(x._1).asInstanceOf[exp],hir_transform(x._2).asInstanceOf[exp])))
			
			case e:PSet => set_exp(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			
			case e:Tuple_or_test => {
				if(e.l.size == 1)
					hir_transform(e.l(0))
				else
					tuple_exp(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			}
			
			case e:Testlist => py_list(e.l map ((x) => hir_transform(x).asInstanceOf[exp]))
			
			case err => {println("invalid syntax node "+err) ; exit(1); null}
		}
	}	
}


object PyTrans {
  def stdin : String = {
    val s = new StringBuilder() ;
    var c = System.in.read()

    while (c != -1) {
      s.append(c.asInstanceOf[Char])
      c = System.in.read()
    }

    s.toString()
  }
	
	

  def main(args : Array[String]) {
	//val input = scala.io.Source.fromFile(args(0)).mkString("")
	val input = stdin	
	//PyParser.print(input) 
	val parsed = PyParser.parse(input)
	val hir_transformed = Hir_transformer.hir_transform(parsed)
	println(hir_transformed)
  }

}
  

