import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._


trait PyTokens extends Tokens {
  case class Newline(dummy:String) extends Token {
    def chars = dummy
    override def toString = "(NEWLINE)"
  }

  case class Indent(count:int) extends Token {
    def chars = ""
    var str = ""
    (1 to count) foreach(i=>str = str+"(INDENT)\n")
    override def toString = str.take(str.length-1)
  }

  case class Dedent(count:int) extends Token {
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
    def chars = dummy
    override def toString = ""
  }
}

    
class PyLexical extends Lexical with PyTokens{
  import scala.util.parsing.input.CharArrayReader.EofCh
  import scala.collection.immutable.HashSet
  import scala.collection.immutable.HashMap
  import scala.collection.mutable.Stack

  var stack : Stack[int] = new Stack()
  stack.push(0)
  
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
/*  def openSquareChange(sign:String) = {val cur:int = counter("[")              
                                      if(sign == add)                         
                                        counter = counter.update("[",cur+1)   
                                      else
                                        counter = counter.update("[",cur-1)   
                                    }  
  def openFlowerChange(sign:String) = {val cur:int = counter("{")             
                                      if(sign == add)
                                        counter = counter.update("{",cur+1)
                                      else
                                        counter = counter.update("{",cur-1)
                                    }


  var counter:Map[String,int] = Map("("->0,"["->0,"{"->0)
*/

var counter:Int = 0
def getSpecialCounter:Boolean = if(counter>0) true else false

/*  def getSpecialCounter:Boolean = if(counter("(") >0) /*|| counter("[") > 0 || counter("{") > 0)*/ 
					return true else return false
*/

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



/*    def fraction = '.' ~ optDigits ^^ {
        case dot ~ ff => dot :: ff :: Nil mkString ""
    }

    def optDigits = opt(rep1(digit)) ^^ {
	case None => ""
	case Some(digits) => digits mkString ""
    }

    def optFraction = opt(fraction) ^^ {
        case None => ""
        case Some(fraction) => fraction
    }
    def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
        case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
    }
    def optExponent = opt(exponent) ^^ {
        case None => ""
        case Some(exponent) => exponent
    }

def floatnumber = (rep1(digit) ~ optFraction ~ optExponent ^^
            { case intPart ~ frac ~ exp => (
                    (intPart mkString "") :: frac :: exp :: Nil mkString "")}
                  |fraction ~ optExponent ^^
            { case frac ~ exp => (
                    frac :: exp :: Nil mkString "")}
            )


def intpart = rep1(digit) ^^ {case ip => ip mkString ""}
*/ 

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
    (elem("", ch=>ch == '\"'|| ch == '\'') ~rep(elem("",ch=>ch!=EofCh&&ch!='\n'&&ch!='\\'&&ch!='\''&&ch!='\"'&&ch!='\r')) ~ (newline|elem("",ch=>ch==EofCh))) ^^ {case a => println("(ERROR \"newline in string\")");exit(1)}
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

def flatten(l: List[Any]): List[Any] = l flatMap { case l: List[_] => flatten(l); case e => List(e); }

  def makeLitString(l:List[Any]):String = {if(l.isEmpty) "" 
                                           else {
                                                if(l.first == ()) makeLitString(l.drop(1))
                                                else   
                                                  l.first.toString+(makeLitString(l.drop(1)))
                                           }
                                          } 
}

object PyParser extends TokenParsers {
  type Tokens = PyTokens
  override val lexical = new PyLexical
  
  import scala.util.parsing.combinator.lexical.Scanners

  def print(s:String) = {
        val tokens:lexical.Scanner = new lexical.Scanner(s)
        var tk:lexical.Scanner = tokens
        while(tk.first.toString == "(NEWLINE)" || tk.first.toString == "")
          tk = tk.rest
        if(tk.first.toString == "(INDENT)")
          {println("(Error - first line indented)"); exit(1)}
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


object Lexer {
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
    PyParser.print(stdin)
    println("(ENDMARKER)")
  }

}
  

