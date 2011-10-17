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
	abstract class PyExp
	
	
	abstract class Factor
	
	case class Factor_unary_op(s:String,f:Factor) extends Factor{
		var str = ""
		if(s == null)
			str = f.toString
		else
			str = "(\""+s+"\" "+ f.toString+")"				
		override def toString = str
	}
	
	case class Power(i:Indexed,f:Factor) extends Factor{
		var str =""
		if(f==null)
			str = i.toString
		else
			str = "(power "+i.toString+" "+f.toString+")"
				
		override def toString = str
	}
	
	case class Indexed(a:Atom,l:List[Trailer]){
		var str = ""
		if(l.size == 0)
			str = a.toString
		else
			str = "(indexed "+a.toString+" "+(l mkString " ")+")"		
		override def toString = str
	}
	
	abstract class Trailer
	
	case class Called(a:Arglist) extends Trailer{
		var str = ""
		if(a != null)
			str = " "+a.toString
		override def toString = "(called"+str+")"
	}
	
	case class Subscript(t:Tuple_or_test) extends Trailer{
		override def toString = "(subscript "+t.toString+")"
	}
	
	case class Dot(s:String) extends Trailer{
		override def toString = "(dot "+s+")"
	}
	
	case class testlist(l:List[Test]){
		override def toString = (l mkString " ")
	}
	
	case class Arglist(l:List[Test]){
		override def toString = (l mkString " ")
	}
	
	
	
	case class Program(stmts:List[Stmt]) extends PyExp{
		override def toString = "(program "+(stmts mkString " ")+")"
	}
	
	abstract class Stmt
	
	case class Simple_stmt(s:List[Small_stmt]) extends Stmt{
		var str = ""
		if(s.size>1)			
			str = "(begin "+(s mkString " ")+")"
		else
			str = s(0).toString
		override def toString = str
	}
	
		
	abstract class Small_stmt 
	
	//case class Expr_stmt(a:Aug_assign, test:List[Test], tuple_or_test:Tuple_or_Test) extends Small_stmt{
		
	abstract class Expr_stmt extends Small_stmt
	
	case class Aug_assign_expr(a:String,l:List[Test],t:Tuple_or_test) extends Expr_stmt{
		override def toString = "(\""+a+"\" ("+(l mkString " ")+")"+t.toString+")"
	}
	
	case class Eq_assign(l:List[Test],t:Tuple_or_test) extends Expr_stmt{
		override def toString = "(= ("+(l mkString " ")+")"+t.toString+")"
	}
	
	case class Expr_assign(t:Tuple_or_test) extends Expr_stmt{
		override def toString = "(expr "+t.toString+")"
	}
	
	
	case class Del_stmt(s:Star_expr) extends Small_stmt{
		override def toString = "(del "+s.toString+")"
	}
	
	
	case class Pass_stmt() extends Small_stmt{
		override def toString = "(pass)"
	}
	
	abstract class Flow_stmt extends Small_stmt
	
	
	case class Global_stmt(l:List[String]) extends Small_stmt{
		override def toString = "(global "+(l mkString " ")+")"
	}
	
	case class Non_local_stmt(l:List[String]) extends Small_stmt{
		override def toString = "(nonlocal "+(l mkString " ")+")"
	}
	
	case class Assert_stmt(t1:Test,t2:Test) extends Small_stmt{
		override def toString = "(assert "+t1.toString+" "+t2.toString+")"
	}
	
	
	case class Break_stmt() extends Flow_stmt{
		override def toString = "(break)"
	}
	
	case class Continue_stmt() extends Flow_stmt{
		override def toString = "(continue)"
	}
	
	case class Return_stmt(l:List[Test]) extends Flow_stmt{
		override def toString = "(return "+(l mkString " ")+")" 
	}
	
	case class Raise_stmt(t1:Test,t2:Test) extends Flow_stmt{
		var t1Str = ""
		var t2Str = ""
		if(t1!=null)
			t1Str = " "+t1.toString
		if(t2!=null)
			t2Str = " "+t2.toString
		override def toString = "(raise"+t1Str+t2Str+")"
	}
	
	
	abstract class Compound_stmt extends Stmt
	
	case class Comp_if_stmt(l:List[(Test,Suite)],e:Suite) extends Compound_stmt{
		var else_str =""
		if(e!=null)
			else_str = " (else "+e+")"
		override def toString = "(cond "+((l map pair_toString) mkString " ")+else_str+")"
	}
	
	case class Comp_while_stmt(t:Test,s1:Suite,s2:Suite) extends Compound_stmt{
		var s2_str = ""
		if(s2 != null)
			s2_str = " "+s2.toString		
		override def toString =  "(while "+t.toString+" "+s1.toString+s2_str+")"
	}
	
	case class Comp_for_stmt(n:String,t:Test,s1:Suite,s2:Suite) extends Compound_stmt{
		var tmp = ""
		if(s2!=null)
			tmp = " "+s2.toString
		override def toString = "(for "+n+" "+t.toString+" "+s1.toString+tmp+")"
	}
	
	case class Func_def(l:List[String],s:Suite) extends Compound_stmt{
		override def toString = "(def ("+(l mkString " ")+")"+s.toString+")"
	}
	
	case class Comp_try_stmt(s:Suite,l:List[(Catch,Suite)],e:Maybe_else,f:Maybe_finally) extends Compound_stmt{
		var newList = "()"
		if(l != null)
			newList = " ("+((l map pair_toString) mkString " ")+")"
		override def toString = "(try "+s.toString+newList+" "+e.toString+" "+f.toString+")"
	}
	
	case class Maybe_else(s:Suite){
		var str = "#f"
		if(s!=null)
			str = s.toString
		override def toString = str
	}
	
	case class Maybe_finally(s:Suite){
		var str = "#f"
		if(s!=null)
			str = s.toString
		override def toString = str
	}
	
	case class Catch(t:Test,n:String){
		var testStr = ""
		var nStr = ""
		if(t != null)
			testStr = " "+t.toString
		if(n != null)
			nStr = " "+n
		override def toString = "(except"+testStr+nStr+")" 
	}
	
	//case class Except()
	
	abstract class Suite
	
	case class Suite_simple(s:Simple_stmt) extends Suite{
		override def toString = s.toString
	}
	
	case class Suite_stmt(l:List[Stmt]) extends Suite{
		override def toString = "(suite "+(l mkString " ")+")"
	}
	
	
	
	abstract class Test
	
	case class If_expr(p:Or_test,c:Or_test,a:Test) extends Test{
		override def toString = "(if "+p.toString+" "+c.toString+" "+a.toString+")"
	}
	
	case class Or_test(l:List[And_test]) extends Test{
		var str = ""
		if(l.size>1)			
			str = "(or "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	case class Lambda_def(l:List[String],t:Test) extends Test{
		override def toString = "(lambda ("+(l mkString " ")+") "+t+")"
	}
	
	case class And_test(l:List[Not_test]){
		var str = ""
		if(l.size>1)			
			str = "(and "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	abstract class Not_test
	
	case class Not_not_test(n:Not_test) extends Not_test{
		override def toString = "(not "+n.toString+")"
	}
	
	case class Comparison(s:Star_expr, l:List[(String,Star_expr)]) extends Not_test {
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
	
	case class Star_expr(star:Boolean,l:Expr){
		var str = ""
		if(!star)
			str = l.toString
		else
			str = "(star "+l.toString+")"
		override def toString = str		
	}
	
	case class Expr(l:List[Xor_expr]){
		var str = ""
		if(l.size>1)			
			str = "(bitwise-or "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class Xor_expr(l:List[And_expr]){
		var str = ""
		if(l.size>1)			
			str = "(bitwise-xor "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class And_expr(l:List[Shift_expr]){
		var str = ""
		if(l.size>1)			
			str = "(bitwise-and "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str	
	}
	
	case class Shift_expr(a:Arith_expr,l:List[(String,Arith_expr)]){
		var str = ""
		if(l.size == 0)
			str = a.toString
		else{
			str = "(shift "+a.toString+" "+((l map pair_toString1) mkString " ")+")"
		}
		override def toString = str
	}
	
	case class Arith_expr(t:Term,l:List[(String,Term)]){
		var str = ""
		if(l.size == 0)
			str = t.toString
		else
			str = "(arith "+t.toString+" "+((l map pair_toString1) mkString " ")+")"
		override def toString = str
	}
	
	case class Term(f1:Factor,l:List[(String,Factor)]){
		var str = ""
		if(l.size == 0)
			str = f1.toString
		else
			str = "(term "+f1.toString+" "+((l map pair_toString1) mkString " ")+")"
		override def toString = str
	}
	
	
	
	
	abstract class Atom
	
	case class Atom_string(s:String) extends Atom{
		override def toString = s 
	}
	
	case class Atom_tuple_or_test(t:Tuple_or_test) extends Atom{
		override def toString = t.toString
	}
	
	case class Atom_testlist (l:Testlist) extends Atom{
		var str = ""
		if(l!=null)
			str = " "+l.toString
		override def toString = "(list"+str+")"
	}
	
	case class Atom_dict(d:Dict_or_set) extends Atom{
		override def toString = d.toString
	}
	
	abstract class Dict_or_set
	
	case class Dict(l:List[(Test,Test)]) extends Dict_or_set{
		override def toString = "(dict "+((l map pair_toString) mkString " ")+")"
	}
	
	case class PSet(l:List[Test]) extends Dict_or_set{
		override def toString = "(set "+(l mkString " ")+")"
	}
	
	/*case class Atom_set(l:List[Test]){
		override def toString = "(set "+listString(l,"")+")"
	}*/
	
	case class Tuple_or_test(l:List[Test]) extends Atom{
		var str = ""
		if(l.size>1)			
			str = "(tuple "+(l mkString " ")+")"
		else
			str = l(0).toString
		override def toString = str
	}
	
	case class Testlist(l:List[Test]) {
		override def toString = (l mkString " ")
	}
	
}

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
    
    
    
    
    
    def parse(s:String) : String = { 
    	program (filter(new lexical.Scanner(s),{t:PyParser.lexical.Token => t.chars != "NULLTOKEN"})) match {
    		case Success(mx, _) => mx.toString
 
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


object PyParse {
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
	//PyParser.print(input) 
	println(PyParser.parse(stdin))    
  }

}
  

