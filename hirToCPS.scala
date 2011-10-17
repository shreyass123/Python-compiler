import scala.util.parsing.combinator._
import scala.util.matching.Regex ;

object HIRParser extends RegexParsers {
	trait HirNode
	case class Program(l:List[Top_form]) extends HirNode{
		override def toString = "(program "+(l mkString " ")+")"
	}
	abstract class Top_form extends HirNode
	
	case class Vardef(v:var_exp,e:Exp) extends Top_form with HirNode {
		override def toString = "(define "+v+" "+e+")"
	}
	case class Exp extends Top_form with HirNode
	case class Multop(op:String) extends HirNode{
		override def toString = op
	}
	case class Binop(op:String) extends HirNode{
		override def toString = op
	}
	case class Unop(op:String) extends HirNode{
		override def toString = op
	}

	case class void_exp extends Exp with HirNode{
		override def toString = "(void)"
	}
	
	case class error_exp(e:Exp) extends Exp with HirNode{
		override def toString = "(error "+e.toString+")"
	}
	
	case class lambda_exp(l:List[var_exp],e:Exp) extends Exp with HirNode{
		override def toString = "(lambda ("+(l mkString " ")+") "+e.toString+")"
	}
	
	case class call_ec_exp(l:lambda_exp) extends Exp with HirNode{
		override def toString = "(call/ec "+l.toString+")"
	}
	
	case class var_exp(s:String) extends Exp with HirNode{
		override def toString = s
	}
	
	case class num_exp(i:String) extends Exp with HirNode{
		override def toString = i 
	}
	
	case class string_exp(s:String) extends Exp with HirNode{
		var str = s
		s match {
			case "#f" => str = "#f" 
			case "#t" => str = "#t"
			case "Ellipsis" => str = "Ellipsis"
			case _ => str= "\""+s+"\""
		}
		override def toString = str
	} 
	
	case class integer_huh extends Exp with HirNode{
		override def toString = "integer?"
	}
	
	case class string_huh extends Exp with HirNode{
		override def toString = "string?"
	}
	
	case class tuple_huh extends Exp with HirNode{
		override def toString = "tuple?"
	}
	
	case class dict_huh extends Exp with HirNode{
		override def toString = "dict?"
	}
	
	case class py_list_huh extends Exp with HirNode{
		override def toString = "py-list?"
	}
	
	case class set_huh extends Exp with HirNode{
		override def toString = "set?" 
	}
	
	
	
	case class set_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(set "+(l mkString " ")+")"
	}
	
	case class py_list_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(py-list* "+(l mkString " ")+")"
	}
	
	case class dict_exp(l:List[(Exp,Exp)]) extends Exp with HirNode{
		override def toString = "(dict "+((l map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+")"
	}
	
	case class tuple_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(tuple "+(l mkString " ")+")"
	}
	
	case class let_exp(binds:List[(var_exp,Exp)], l:List[Exp]) extends Exp with HirNode{
		override def toString = "(let ("+((binds map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+
								") "+(l mkString " ")+")"
	}
	
	case class set_bang_exp(v:var_exp,e:Exp) extends Exp with HirNode{
		override def toString = "(set! "+v.toString+" "+e.toString+")"
	}
	
	case class py_list_ref(e1:Exp, e2:Exp) extends Exp with HirNode{
		override def toString = "(py-list-ref "+e1.toString+" "+e2.toString+")"
	}
	
	case class py_list_set_bang(e1:Exp, e2:Exp, e3:Exp) extends Exp with HirNode{
		override def toString = "(py-list-set! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class py_list_remove_bang(e1:Exp, e2:Exp) extends Exp with HirNode{
		override def toString = "(py-list-remove! "+e1.toString+" "+e2.toString+")"
	}
	
	case class tuple_ref(e1:Exp, e2:Exp) extends Exp with HirNode{
		override def toString = "(tuple-ref "+e1.toString+" "+e2.toString+")"
	}
	
	case class tuple_set_bang(e1:Exp, e2:Exp, e3:Exp) extends Exp with HirNode{
		override def toString = "(tuple-set! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class dict_ref(e1:Exp, e2:Exp) extends Exp with HirNode{
		override def toString = "(dict-ref "+e1.toString+" "+e2.toString+")"
	}
	
	case class dict_set_bang(e1:Exp, e2:Exp, e3:Exp) extends Exp with HirNode{
		override def toString = "(dict-set! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class dict_remove_bang(e1:Exp, e2:Exp) extends Exp with HirNode{
		override def toString = "(dict-remove! "+e1.toString+" "+e2.toString+")"
	}
	
	case class get_field(e1:Exp, e2:var_exp) extends Exp with HirNode{
		override def toString = "(get-field "+e1.toString+" "+e2.toString+")"
	}
	
	case class set_field_bang(e1:Exp, e2:var_exp, e3:Exp) extends Exp with HirNode{
		override def toString = "(set-field! "+e1.toString+" "+e2.toString+" "+e3.toString+")"
	}
	
	case class remove_field_bang(e1:Exp, e2:var_exp) extends Exp with HirNode{
		override def toString = "(remove-field! "+e1.toString+" "+e2.toString+")"
	}
	
	case class get_global(v:var_exp) extends Exp with HirNode{
		/*var str = ""
		if(v.isInstanceOf[num_exp])
			str = v.toString
		else
			str = "(get-global "+v.toString+")"
		*/
		override def toString = "(get-global "+v.toString+")"
	}
	
	case class set_global_bang(v:var_exp,e:Exp) extends Exp with HirNode{
		override def toString = "(set-global! "+v.toString+" "+e.toString+")"
	}
	
	case class throw_exp(e:Exp) extends Exp with HirNode{
		override def toString = "(throw "+e.toString+")"
	}
	
	case class try_exp(e1:Exp,e2:Exp) extends Exp with HirNode{
		override def toString = "(try "+e1.toString+" "+e2.toString+")"
	}
	
	case class assert_exp(e1:Exp,e2:Exp) extends Exp with HirNode{
		var str = ""
		if(e2!=null)
			str = " "+e2.toString
		override def toString = "(assert "+e1.toString+str+")"
	}
	
	case class cond_exp(conds0:List[(Exp,Exp)], els0:Exp) extends Exp with HirNode{
		var conds = conds0
		val els = conds0(conds0.length-1)._1 match{
			case v:var_exp => if(v.s.equals("else")){
								
								conds = conds0.take(conds0.length-1)
								conds0(conds0.length-1)._2
							  }
							else null
			case _ => null
		}
								
		var str = ""
		if(els!=null)
			str = " (else "+els.toString+")"
		override def toString = "(cond "+((conds map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+
							str+")"
	}
	
	case class if_exp(a:Exp,b:Exp,c:Exp) extends Exp with HirNode{
		override def toString = "(if "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class and_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(and "+(l mkString " ")+")"
	}
	
	case class or_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(or "+(l mkString " ")+")"
	}
	
	case class not_exp(e:Exp) extends Exp with HirNode{
		override def toString = "(not "+e.toString+")"
	}
	
	case class while_exp(a:Exp,b:Exp,c:Exp) extends Exp with HirNode{
		var str = ""
		if(c!=null)
			str = " "+c.toString
		override def toString = "(while "+a.toString+" "+b.toString+str+")"
	}
	
	case class for_each_exp(v:var_exp,a:Exp,b:Exp,c:Exp) extends Exp with HirNode{
		var str = ""
		if(c!=null)
			str = " "+c.toString
		override def toString = "(for-each "+v.toString+" "+a.toString+" "+b.toString+str+")"
	}
	
	case class break_exp extends Exp with HirNode{
		override def toString = "(break)"
	}
	
	case class continue_exp extends Exp with HirNode{
		override def toString = "(continue)"
	}
	
	case class func_app_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "("+(l mkString " ")+")"
	}
	
	case class py_print(e:Exp) extends Exp with HirNode{
		override def toString = "(py-print "+e.toString+")"
	}
	
	
	
	case class begin_exp(l:List[Exp]) extends Exp with HirNode{
		override def toString = "(begin "+(l mkString " ")+")"
	}
	
	case class multop_exp(m:Multop,l:List[Exp]) extends Exp with HirNode{
		override def toString = "("+m.toString+" "+(l mkString " ")+")"
	}
	
	case class binop_exp(b:Binop,e1:Exp,e2:Exp) extends Exp with HirNode{
		override def toString = "("+b.toString+" "+e1.toString+" "+e2.toString+")" 
	}
	
	case class unop_exp(u:Unop,e:Exp) extends Exp with HirNode{
		override def toString = "("+u.toString+" "+e.toString+")" 
	}
	
	override protected val whiteSpace = ("([\\r\\n\\t ]*)").r
	
	override def skipWhitespace = true
	
	def lpar : Parser[String] = regex(new Regex("[(]")) ^^ { case "(" => "(" }

    def rpar : Parser[String]= regex(new Regex("[)]")) ^^ { case ")" => ")" }
	
	def program_parser:Parser[HirNode] = lpar ~> regex(new Regex("program")) ~> rep(top_form_parser) <~ rpar ^^ {case a => Program(a)}
	
	def top_form_parser = (vardef_parser|exp_parser) ^^ {case a => a}
	
	def vardef_parser = lpar ~> regex(new Regex("define")) ~> var_parser ~ exp_parser <~ rpar ^^ {case a~b => Vardef(a,b)}
	
	def var_parser:Parser[var_exp] = regex(new Regex("[a-zA-Z_][a-zA-Z0-9_]*")) ^^ {case v => var_exp(v)}
	
	def number_parser = regex(new Regex("[0-9]+")) ^^ {case n => num_exp(n)}
	
	def string_parser = regex(new Regex("\"")) ~> regex(new Regex("[^\"\r\n]+")) <~ regex(new Regex("\"")) ^^ {case s => string_exp(s)}
	//def string_parser = """\"""".r ~> regex(new Regex("[^\"\r\n]+")) <~ """\"""".r ^^ {case s => string_exp(s)}
	
	def opt_exp_parser:Parser[Exp] = opt(exp_parser) ^^ {
		case Some(a) => a
		case None => null
	} 
	
	def opt_else_exp = opt(lpar ~> regex(new Regex("else")) ~> exp_parser <~ rpar) ^^{
		case Some(a) => a
		case None => null
	}
	
	def exp_tuple_parser = lpar ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => (a,b)}
	def var_exp_tuple_parser = lpar ~> var_parser ~ exp_parser <~ rpar ^^ {case a~b => (a,b)}
	
	def lambda_parser:Parser[Exp] = lpar ~> regex(new Regex("lambda")) ~> (lpar ~> rep(var_parser) <~ rpar) ~ exp_parser <~ rpar ^^ 
					 												{case b~d => lambda_exp(b,d)}
	
	def exp_parser:Parser[Exp] = (
	
					 lpar ~> regex(new Regex("void")) <~ rpar ^^ {case a => void_exp()}|
					 lpar ~> regex(new Regex("error")) ~> exp_parser <~ rpar ^^ {case a => error_exp(a)}|
					 lpar ~> regex(new Regex("call/ec")) ~> lpar ~> regex(new Regex("lambda")) ~> (lpar ~> var_parser <~ rpar) ~ 
					 		exp_parser <~ rpar <~ rpar ^^ {
					 				case a~b => call_ec_exp(lambda_exp(List(a),b))
					  		}|
					 lambda_parser|					 
					 
					 regex(new Regex("integer[?]")) ^^ {case a => integer_huh()}|
					 regex(new Regex("string[?]")) ^^ {case a => string_huh()}|
					 regex(new Regex("tuple[?]")) ^^ {case a => tuple_huh()}|
					 regex(new Regex("dict[?]")) ^^ {case a => dict_huh()}|
					 regex(new Regex("py[-]list[?]")) ^^ {case a => py_list_huh()}|
					 regex(new Regex("set[?]")) ^^ {case a => set_huh()}|
					 lpar ~> regex(new Regex("set")) ~> rep(exp_parser) <~ rpar ^^ {case a => set_exp(a)}|
					 lpar ~> regex(new Regex("tuple")) ~> rep(exp_parser) <~ rpar ^^ {case a => tuple_exp(a)}|
					 lpar ~> regex(new Regex("py[-]list[*]")) ~> rep(exp_parser) <~ rpar ^^ {case a => py_list_exp(a)}|
					 lpar ~> regex(new Regex("dict")) ~> rep(exp_tuple_parser) <~ rpar ^^ {case a => dict_exp(a)}|
					 lpar ~> regex(new Regex("let")) ~> lpar ~ rep(var_exp_tuple_parser) ~ rpar ~ rep(exp_parser) <~ rpar ^^ 
					 										{case a~b~c~d => let_exp(b,d)}|
					 lpar ~> regex(new Regex("set!")) ~> var_parser ~ exp_parser <~ rpar ^^ {case a~b => set_bang_exp(a,b)}| 
					 lpar ~> regex(new Regex("py[-]list[-]ref")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => py_list_ref(a,b)}|
					 lpar ~> regex(new Regex("py[-]list[-]set!")) ~> exp_parser ~ exp_parser ~ exp_parser <~ rpar ^^ {case a~b~c => py_list_set_bang(a,b,c)}|
					 lpar ~> regex(new Regex("py[-]list[-]remove!")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => py_list_remove_bang(a,b)}|
					 
					 lpar ~> regex(new Regex("tuple[-]ref")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => tuple_ref(a,b)}|
					 lpar ~> regex(new Regex("tuple[-]set!")) ~> exp_parser ~ exp_parser ~ exp_parser <~ rpar ^^ {case a~b~c => tuple_set_bang(a,b,c)}|
					 
					 lpar ~> regex(new Regex("dict[-]ref")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => dict_ref(a,b)}|
					 lpar ~> regex(new Regex("dict[-]set!")) ~> exp_parser ~ exp_parser ~ exp_parser <~ rpar ^^ {case a~b~c => dict_set_bang(a,b,c)}|
					 lpar ~> regex(new Regex("dict[-]remove!")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => dict_remove_bang(a,b)}|
					 
					 lpar ~> regex(new Regex("get[-]field")) ~> exp_parser ~ var_parser <~ rpar ^^ {case a~b => get_field(a,b)}|
					 lpar ~> regex(new Regex("set[-]field!")) ~> exp_parser ~ var_parser ~ exp_parser <~ rpar ^^ {case a~b~c => set_field_bang(a,b,c)}|
					 lpar ~> regex(new Regex("remove[-]field!")) ~> exp_parser ~ var_parser <~ rpar ^^ {case a~b => remove_field_bang(a,b)}|
					 
					 lpar ~> regex(new Regex("get[-]global")) ~> var_parser <~ rpar ^^ {case a => get_global(a)}|
					 lpar ~> regex(new Regex("set[-]global!"))  ~> var_parser ~ exp_parser <~ rpar ^^ {case a~b => set_global_bang(a,b)}|
					 
					 lpar ~> regex(new Regex("throw")) ~> exp_parser <~ rpar ^^ {case a => throw_exp(a)}|
					 lpar ~> regex(new Regex("try")) ~> exp_parser ~ exp_parser <~ rpar ^^ {case a~b => try_exp(a,b)}|
					 lpar ~> regex(new Regex("assert")) ~> exp_parser ~ opt_exp_parser <~ rpar ^^ {case a~b => assert_exp(a,b)}|
					 
					 lpar ~> regex(new Regex("cond")) ~> rep(exp_tuple_parser) ~ opt_else_exp <~ rpar ^^ {
						 case a ~ b => cond_exp(a,b)
					 }|
					 
					 lpar ~> regex(new Regex("if")) ~> exp_parser ~ exp_parser ~ exp_parser <~ rpar ^^ {case a~b~c => if_exp(a,b,c)}|
					 lpar ~> regex(new Regex("and")) ~> rep(exp_parser) <~ rpar ^^ {case a => and_exp(a)}|
					 lpar ~> regex(new Regex("or")) ~> rep(exp_parser) <~ rpar ^^ {case a => or_exp(a)}|
					 lpar ~> regex(new Regex("not")) ~> exp_parser <~ rpar ^^ {case a => not_exp(a)}|
					 lpar ~> regex(new Regex("while")) ~> exp_parser ~ exp_parser ~ opt_exp_parser <~ rpar ^^ {
						 case a~b~c => while_exp(a,b,c)
					 }|
					 lpar ~> regex(new Regex("for[-]each")) ~> var_parser ~ exp_parser ~ exp_parser ~ opt_exp_parser <~ rpar ^^ {						 
						 case a~b~c~d => for_each_exp(a,b,c,d)
					 }|
					 lpar ~> regex(new Regex("break")) <~ rpar ^^ {case a => break_exp()}|
					 lpar ~> regex(new Regex("continue")) <~ rpar ^^ {case a => continue_exp()}|
					 lpar ~> regex(new Regex("begin")) ~> rep(exp_parser) <~ rpar ^^ {case a => begin_exp(a)}|
					 lpar ~> binop_parser ~ exp_parser ~ exp_parser <~ rpar ^^ {case a~b~c => binop_exp(a,b,c)}|	
					 lpar ~> multop_parser ~ rep(exp_parser) <~ rpar ^^ {case a~b => multop_exp(a,b)}|
					 lpar ~> unop_parser ~ exp_parser <~ rpar ^^ {case a~b => unop_exp(a,b)}|
					 
					 lpar ~> "py[-]print".r ~> exp_parser <~ rpar ^^ {case a => py_print(a)}|
					 lpar ~> rep(exp_parser) <~ rpar ^^ {case a => func_app_exp(a)}|
					 regex(new Regex("Exception|Object|Ellipsis|#t|#f|[']None")) ^^ {case a => string_exp(a)}|
					 (var_parser|number_parser|string_parser)  ^^ {case a => a}
					 
					 
					 )
					 
	def multop_parser = regex(new Regex("[&]|[\\^]")) ^^ {case a => Multop(a)}
	def binop_parser = regex(new Regex("[+]|<=|>=|[<>]|[*]|[/-]|>>|<<|equal[?]|quotient|modulo|not-equal[?]|in[?]|not-in[?]|eq[?]|not-eq[?]|expt")) ^^ {case a => Binop(a)}
	def unop_parser = regex(new Regex("bitwise-not|[+]|[-]")) ^^ {case a => Unop(a)}
	
	def parseHir (input:String)= {
		parse(program_parser,input) match{
			case Success(a,_) => a
			case Failure(msg,next) => throw new Exception("parse failed: " + msg+" "+next.pos.line+" "+next.pos.column+" "+next.pos.longString)
			case _ => throw new Exception("error")
		}
	}
}


object LIR {
	import HIRParser._
	
	abstract trait LirNode

	case class program_lir(l:List[vardef_lir], e:exp_lir) extends LirNode{
		override def toString = "(program "+(l mkString " ")+" "+e.toString+")"
	}
	
	case class vardef_lir(v:var_lir,a:aexp_lir) extends LirNode{
		override def toString = "(define "+v.toString+" "+a.toString+")"
	}
	
	abstract class aexp_lir extends exp_lir with LirNode{
		override def isAtomic = true
	}
	
	case class void_lir extends aexp_lir with LirNode{
		override def toString = "(void)"
	}
	
	case class var_lir(s:String) extends aexp_lir with LirNode{
		override def toString = s
	}
	
	case class num_lir(i:String) extends aexp_lir with LirNode{
		override def toString = i 
	}
	
	case class string_lir(s:String) extends aexp_lir with LirNode{
		override def toString = s
	}
	
	case class lambda_lir(l:List[var_lir],e:exp_lir) extends aexp_lir with LirNode{
		override def toString = "(lambda ("+(l mkString " ")+") "+e.toString+")"
	}
	
	abstract class exp_lir extends LirNode{
		def isAtomic = false
	}
	
	case class aexp_exp_lir(a:aexp_lir) extends exp_lir with LirNode{
		override def isAtomic = true
		override def toString = a.toString
	}
	
	case class error_lir(e:exp_lir) extends exp_lir with LirNode{
		var str =  "(error "+e.toString+")"
		if(e.isInstanceOf[string_lir])
			str =  "(error \""+e.toString+"\")"
		override def toString = str
	}
	
	case class call_ec_lir(e:exp_lir) extends exp_lir with LirNode{
		//override def isAtomic = true
		override def toString = "(call/ec "+e.toString+")"
	}
	
	case class if_lir(a:exp_lir,b:exp_lir,c:exp_lir) extends exp_lir with LirNode{
		override def toString = "(if "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class set_bang_lir(v:var_lir,e:exp_lir) extends exp_lir with LirNode{
		override def toString ="(set! "+v.toString+" "+e.toString+")"
	}
	
	case class begin_lir(l:List[exp_lir]) extends exp_lir with LirNode{
				override def toString = "(begin "+(l mkString " ")+")"
	}
	
	case class func_app_lir(l:List[exp_lir]) extends exp_lir with LirNode{
		override def toString ="("+(l mkString " ")+")"
	}
	
	case class set_lir(l:List[exp_lir]) extends exp_lir with LirNode{
		override def isAtomic = true
		override def toString = "(set "+(l mkString " ")+")"
	}
	
	case class tuple_lir(l:List[exp_lir]) extends exp_lir with LirNode{
		override def isAtomic = true
		override def toString = "(tuple "+(l mkString " ")+")"
	}
	
	case class py_list_lir(l:List[exp_lir]) extends exp_lir with LirNode{
		override def isAtomic = true
		override def toString = "(py-list* "+(l mkString " ")+")"
	}
	
	case class dict_lir(l:List[(exp_lir,exp_lir)]) extends exp_lir with LirNode{
		override def isAtomic = true
		override def toString = "(dict "+((l map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+")"
	}
	
	case class triop_exp_lir(op:triop_lir,a:exp_lir,b:exp_lir,c:exp_lir) extends exp_lir with LirNode{
		override def toString = "("+op.toString+" "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class binop_exp_lir(op:binop_lir,a:exp_lir,b:exp_lir) extends exp_lir with LirNode{
		override def toString = "("+op.toString+" "+a.toString+" "+b.toString+")"
	}
	
	case class unop_exp_lir(op:unop_lir,a:exp_lir) extends exp_lir with LirNode{
		var str = "("+op.toString+" "+a.toString+")"
		if(op.op.equals("py-print") && a.isInstanceOf[string_lir])
			str = "("+op.toString+" \""+a.toString+"\")"	
		override def toString = str
	}
	
	case class triop_lir(op:String) extends LirNode{
		override def toString = op
	}
	
	case class binop_lir(op:String) extends LirNode{
		override def toString = op
	}
	
	case class unop_lir(op:String) extends LirNode{
		override def toString = op
	}
	
	var ec_counter = 14
	var ex_counter = 13
	
	
	def get_next_ec_var = {
		ec_counter = ec_counter + 1
		"$ec"+ec_counter
	}
	
	def get_next_ex_var = {
		ex_counter = ex_counter + 1
		"$ex"+ex_counter
	}
	
	var gensym_counter = 0
	var gensym_alpha:Char = 'a'
	
	def gensym:String = {
		if(gensym_counter == 99){
			gensym_counter = 0
			gensym_alpha = (gensym_alpha+1).asInstanceOf[Char]
			
		}
		gensym_counter+=1
		return "$"+gensym_alpha.toString+gensym_counter
		
	}
	
	def convert (in: HIRParser.HirNode):LirNode = {
		in match{
				 case h:Program =>{
					 var vars:List[vardef_lir] = List()
					 var expr:List[exp_lir] = List()
					 for(i<-h.l){
						 i match{
							 case v:Vardef => vars =convert(v).asInstanceOf[vardef_lir]::vars
							 case e:Exp => expr = convert(e).asInstanceOf[exp_lir]::expr 
						 }
					 }
					 vars = List(vardef_lir(var_lir("break"),void_lir()),vardef_lir(var_lir("return"),void_lir()),
					 vardef_lir(var_lir("continue"),void_lir()),vardef_lir(var_lir("$current-handler"),void_lir()))++vars.reverse
					 program_lir(vars,begin_lir(expr.reverse))
				 }
				
				case h:Vardef => {
					vardef_lir(var_lir("g$"+h.v.s), convert(h.e).asInstanceOf[aexp_lir])
				}
	
				case h:Multop => {
					binop_lir(h.op) //all multops are binops in lir
				}
				case h:Binop => {
					binop_lir(h.op)
				}
				case h:Unop=> {
					unop_lir(h.op)
				}

				case h:void_exp => void_lir()
	
				case h:error_exp=> {
					error_lir(convert(h.e).asInstanceOf[exp_lir])
				}
	
				case h:lambda_exp=> {
					val vars = h.l map ((x) => var_lir(x.s))
					val expr = convert(h.e).asInstanceOf[exp_lir]
					lambda_lir(vars,expr)
				}
	
				case h:call_ec_exp=> call_ec_lir(convert(h.l).asInstanceOf[exp_lir])
	
				case h:var_exp=> var_lir(h.s)
	
				case h:num_exp=> num_lir(h.i)
	
				case h:string_exp=> string_lir(h.s)
	
				case h:integer_huh=> unop_lir("integer?")
	
				case h:string_huh=> unop_lir("string?")
	
				case h:tuple_huh=> unop_lir("tuple?")
	
				case h:dict_huh => unop_lir("dict?")
	
				case h:py_list_huh=> unop_lir("py-list?")
	
				case h:set_huh => unop_lir("set?")
	
				case h:set_exp=> {
					val setlist:List[exp_lir] = h.l map ((x) => convert(x).asInstanceOf[exp_lir])
					set_lir(setlist)
				}
	
				case h:py_list_exp=> {
					val pylist:List[exp_lir] = h.l map ((x) => convert(x).asInstanceOf[exp_lir])
					py_list_lir(pylist)
				}
	
				case h:dict_exp=> {
					val dictlist:List[(exp_lir,exp_lir)] = h.l map ((x) => (convert(x._1).asInstanceOf[exp_lir],convert(x._2).asInstanceOf[exp_lir]))
					dict_lir(dictlist)
				}
	
				case h:tuple_exp=> {
					val tuplist:List[exp_lir] = h.l map ((x) => convert(x).asInstanceOf[exp_lir])
					tuple_lir(tuplist)
				}
	
				case h:let_exp => {
					val let_vars = h.binds map ((x) => convert(x._1).asInstanceOf[var_lir])
					val let_vals = h.binds map ((x) => convert(x._2).asInstanceOf[exp_lir])
					val let_body = h.l map ((x) => convert(x).asInstanceOf[exp_lir])
					val lambda_body = begin_lir(let_body)
					val lambda_expression = lambda_lir(let_vars,lambda_body).asInstanceOf[exp_lir]
					return func_app_lir(lambda_expression::let_vals)					
				}
	
				case h:set_bang_exp=> {
					set_bang_lir(convert(h.v).asInstanceOf[var_lir],convert(h.e).asInstanceOf[exp_lir])
				}
	
				case h:py_list_ref=> {
					val binop_ = binop_lir("py-list-ref")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					binop_exp_lir(binop_,exp1,exp2)
				}
	
				case h:py_list_set_bang=> {
					val triop_ = triop_lir("py-list-set!")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					val exp3 = convert(h.e3).asInstanceOf[exp_lir]
					triop_exp_lir(triop_,exp1,exp2,exp3)
				}
	
				case h:py_list_remove_bang=> {
					val binop_ = binop_lir("py-list-remove!")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					binop_exp_lir(binop_,exp1,exp2)
				}
	
				case h:tuple_ref=> {
					val binop_ = binop_lir("tuple-ref")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					binop_exp_lir(binop_,exp1,exp2)
				}
	
				case h:tuple_set_bang=> {
					val triop_ = triop_lir("tuple-set!")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					val exp3 = convert(h.e3).asInstanceOf[exp_lir]
					triop_exp_lir(triop_,exp1,exp2,exp3)
				}
	
				case h:dict_ref=> {
					val binop_ = binop_lir("dict-ref")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					binop_exp_lir(binop_,exp1,exp2)
				}
	
				case h:dict_set_bang=> {
					val triop_ = triop_lir("dict-set!")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					val exp3 = convert(h.e3).asInstanceOf[exp_lir]
					triop_exp_lir(triop_,exp1,exp2,exp3)
				}
	
				case h:dict_remove_bang=> {
					val binop_ = binop_lir("dict-remove!")
					val exp1 = convert(h.e1).asInstanceOf[exp_lir]
					val exp2 = convert(h.e2).asInstanceOf[exp_lir]
					binop_exp_lir(binop_,exp1,exp2)
				}
	
				case h:get_field=> null
	
				case h:set_field_bang=> null
	
				case h:remove_field_bang=> null
	
				case h:get_global=> {
					//val var_ = convert(h.v).asInstanceOf[var_lir]
					var_lir("g$"+h.v.s)					
				}
					
	
				case h:set_global_bang=> {
					set_bang_lir(var_lir("g$"+h.v.s),convert(h.e).asInstanceOf[exp_lir])
				}
	
				case h:throw_exp=> {
					func_app_lir(List(var_lir("$current-handler"), convert(h.e).asInstanceOf[exp_lir]))
				}
	
				case h:try_exp=> {
					val new_ec = get_next_ec_var
					val new_ex = get_next_ex_var
					val inner_most_let_body = 
						call_ec_lir(
								lambda_lir(List(var_lir(new_ec)), 
									(begin_lir(List(
										(set_bang_lir(var_lir("$current-handler"),
											lambda_lir(List(var_lir(new_ex)), 
												begin_lir(List(set_bang_lir(var_lir("$current-handler"), var_lir("$old-handler")),
													(func_app_lir(List(var_lir(new_ec), func_app_lir(List(convert(h.e2).asInstanceOf[exp_lir], var_lir(new_ex))))))))))),													
									func_app_lir(List(lambda_lir(List(var_lir("rv")), begin_lir(List(begin_lir(List(set_bang_lir(var_lir("$current-handler"), var_lir("$old-handler")), var_lir("rv")))))), 
												convert(h.e1).asInstanceOf[exp_lir])))))))
										   
										
								
					val break_binding = lambda_lir(List(), 
							begin_lir(List(set_bang_lir(var_lir("$current-handler"), var_lir("$old-handler")),
								(func_app_lir(List(var_lir("$old-break")))))))
								
					val continue_binding = lambda_lir(List(), 
							begin_lir(List(set_bang_lir(var_lir("$current-handler"), var_lir("$old-handler")),
								(func_app_lir(List(var_lir("$old-continue")))))))
								
					val return_binding = lambda_lir(List(var_lir("rv")), 
							begin_lir(List(set_bang_lir(var_lir("$current-handler"), var_lir("$old-handler")),
								(func_app_lir(List(var_lir("return"),var_lir("rv")))))))
								
					val lambda_break_app = func_app_lir(List(lambda_lir(List(var_lir("break")), begin_lir(List(inner_most_let_body))), break_binding))
					val lambda_continue_app = func_app_lir(List(lambda_lir(List(var_lir("continue")), begin_lir(List(lambda_break_app))), continue_binding))
					
					val lambda_return_app = func_app_lir(List(lambda_lir(List(var_lir("return")), begin_lir(List(lambda_continue_app))), return_binding))
					
					val lambda_old_break_app = func_app_lir(List(lambda_lir(List(var_lir("$old-break")), begin_lir(List(lambda_return_app))),var_lir("break")))
					val lambda_old_continue_app = func_app_lir(List(lambda_lir(List(var_lir("$old-continue")), begin_lir(List(lambda_old_break_app))),var_lir("continue")))
					val lambda_old_return_app = func_app_lir(List(lambda_lir(List(var_lir("$old-return")), begin_lir(List(lambda_old_continue_app))),var_lir("return")))
					val lambda_old_handler_app = func_app_lir(List(lambda_lir(List(var_lir("$old-handler")), begin_lir(List(lambda_old_return_app))),var_lir("$current-handler")))
					lambda_old_handler_app
				}
	
				case h:assert_exp=> {
					if(h.e2==null){
						unop_exp_lir(unop_lir("assert1"),lambda_lir(List(),convert(h.e1).asInstanceOf[exp_lir]))
					}
					else{
						binop_exp_lir(binop_lir("assert2"), lambda_lir(List(), convert(h.e1).asInstanceOf[exp_lir]), 
															lambda_lir(List(), convert(h.e2).asInstanceOf[exp_lir]))
					}
				}
	
				case h:cond_exp=> {
					var exp_list_reverse = h.conds.reverse
					var prev_if_exp:if_lir = null
					
					if(exp_list_reverse.length == 0){
						if(h.els == null)
							return void_lir()
						else
							return convert(h.els)
					}
					
					if(h.els != null)
						prev_if_exp = if_lir(convert(exp_list_reverse(0)._1).asInstanceOf[exp_lir], convert(exp_list_reverse(0)._2).asInstanceOf[exp_lir], convert(h.els).asInstanceOf[exp_lir])
					else
						prev_if_exp = if_lir(convert(exp_list_reverse(0)._1).asInstanceOf[exp_lir], convert(exp_list_reverse(0)._2).asInstanceOf[exp_lir], void_lir())
					
					exp_list_reverse = exp_list_reverse.drop(1)
					for(i<-exp_list_reverse){
						prev_if_exp = if_lir(convert(i._1).asInstanceOf[exp_lir], convert(i._2).asInstanceOf[exp_lir], prev_if_exp)						
					}
					prev_if_exp
				}
	
				case h:if_exp=> {
					if_lir(convert(h.a).asInstanceOf[exp_lir],convert(h.b).asInstanceOf[exp_lir],convert(h.c).asInstanceOf[exp_lir])
				}
	
				case h:and_exp=> {
					if(h.l.size == 1)
						return convert(h.l(0)).asInstanceOf[exp_lir]
					val exp_list = h.l.reverse
					var prev_stmt:if_lir = if_lir(convert(exp_list(1)).asInstanceOf[exp_lir],convert(exp_list(0)).asInstanceOf[exp_lir], string_lir("#f"))
					for(i<-exp_list.drop(2)){
						prev_stmt = if_lir(convert(i).asInstanceOf[exp_lir],prev_stmt, var_lir("#f"))
					}
					prev_stmt
				}
	
				case h:or_exp=> {
					if(h.l.length == 1)
						return convert(h.l(0)).asInstanceOf[exp_lir]
					val stmt_list_rev = h.l.reverse
					
					val last_exp = convert(stmt_list_rev(0)).asInstanceOf[exp_lir]
					val second_last_exp = convert(stmt_list_rev(1)).asInstanceOf[exp_lir]
					val new_sym = var_lir(gensym)
					var prev_exp = func_app_lir(List(lambda_lir(List(new_sym),begin_lir(List(if_lir(new_sym,new_sym,last_exp)))), second_last_exp))
					
					for(i<-stmt_list_rev.drop(2)){
						val cur_exp = convert(i).asInstanceOf[exp_lir]
						val new_sym = var_lir(gensym)
						
						prev_exp = func_app_lir(List(lambda_lir(List(new_sym),begin_lir(List(if_lir(new_sym,new_sym,prev_exp)))), cur_exp))
					}
					prev_exp
				}
	
				case h:not_exp=> unop_exp_lir(unop_lir("not"),convert(h.e).asInstanceOf[exp_lir])
	
				case h:while_exp=> {
					val while_cond = convert(h.a).asInstanceOf[exp_lir]
					val while_body = convert(h.b).asInstanceOf[exp_lir]
					var while_else:exp_lir = void_lir()
					if(h.c!=null)
						while_else = convert(h.c).asInstanceOf[exp_lir]
					
					call_ec_lir(lambda_lir(List(var_lir("break")),
								  func_app_lir(List(lambda_lir(List(var_lir("loop")),
								 		         begin_lir(List(set_bang_lir(var_lir("loop"),
								 		        		 	lambda_lir(List(), if_lir(while_cond, 
								 		        		 								begin_lir(List(call_ec_lir(lambda_lir(List(var_lir("continue")),while_body)), 
								 		        		 										   func_app_lir(List(var_lir("loop"))))),
								 		        		 								void_lir()))),
								 		        		 	func_app_lir(List(var_lir("loop"))),
								 		        		 	(while_else )))),
								 		       void_lir()))))
						
						
				}
	
				case h:for_each_exp => {
					val for_var = var_lir(h.v.s)
					val for_seq = convert(h.a).asInstanceOf[exp_lir]
					val for_body = convert(h.b).asInstanceOf[exp_lir]
					var for_else:exp_lir = void_lir()
					if(h.c!=null)
						for_else = convert(h.c).asInstanceOf[exp_lir]
					val seq_var = gensym
					val loop_var = gensym
					call_ec_lir(lambda_lir(List(var_lir("break")), 
									func_app_lir(List(lambda_lir(List(var_lir(seq_var),var_lir(loop_var)),
														(begin_lir(List(begin_lir(List(if_lir(unop_exp_lir(unop_lir("set?"), var_lir(seq_var)),
																							  binop_exp_lir(binop_lir("for-set-k"), var_lir(seq_var), var_lir(loop_var)),
																							  if_lir(unop_exp_lir(unop_lir("tuple?"), var_lir(seq_var)),
																							 		 binop_exp_lir(binop_lir("for-tuple-k"), var_lir(seq_var), var_lir(loop_var)),
																							 		 if_lir(unop_exp_lir(unop_lir("py-list?"), var_lir(seq_var)),
																							 				 binop_exp_lir(binop_lir("for-py-list-k"), var_lir(seq_var), var_lir(loop_var)),
																							 				 if_lir(unop_exp_lir(unop_lir("dict?"), var_lir(seq_var)),
																							 				 		binop_exp_lir(binop_lir("for-dict-k"), var_lir(seq_var), var_lir(loop_var)),
																							 				 		void_lir())))),
																					   for_else)))))),
													 for_seq,
													 lambda_lir(List(for_var),
															 	call_ec_lir(lambda_lir(List(var_lir("continue")),
															 						   for_body)))))))
																							 				 			  
				
				}
	
				case h:break_exp => func_app_lir(List(var_lir("break")))
	
				case h:continue_exp => func_app_lir(List(var_lir("continue")))
	
				case h:func_app_exp=> {
					val func_name = convert(h.l(0))
					if(func_name.isInstanceOf[unop_lir])
						return unop_exp_lir(func_name.asInstanceOf[unop_lir], convert(h.l(1)).asInstanceOf[exp_lir])
					func_app_lir(h.l map ((x) => convert(x).asInstanceOf[exp_lir]))
				}
	
				case h:py_print=> unop_exp_lir(unop_lir("py-print"),convert(h.e).asInstanceOf[exp_lir])
	
				case h:begin_exp=> begin_lir(h.l map ((x) => convert(x).asInstanceOf[exp_lir]))
	
				case h:multop_exp=> {
					if(h.l.length < 2)
						throw new Exception("exception in multop")
					
					var prev_term:binop_exp_lir = binop_exp_lir(binop_lir(h.m.op), convert(h.l(h.l.size-1)).asInstanceOf[exp_lir], convert(h.l(h.l.size-2)).asInstanceOf[exp_lir]) 
					
					for(i<-h.l.take(h.l.size-2).reverse){
						prev_term = binop_exp_lir(binop_lir(h.m.op),convert(i).asInstanceOf[exp_lir],prev_term)
					}
					prev_term
				}
	
				case h:binop_exp=> binop_exp_lir(binop_lir(h.b.op),convert(h.e1).asInstanceOf[exp_lir],convert(h.e2).asInstanceOf[exp_lir])
	
				case h:unop_exp=> unop_exp_lir(unop_lir(h.u.op),convert(h.e).asInstanceOf[exp_lir])

		}
	}
}

object CPS{
	import LIR._
	
	abstract trait CPSNode{
		def isAtomic:Boolean = false
	}
	case class program_cps(l:List[def_cps], e:cexp_cps) extends CPSNode{
		override def toString = "(program "+(l mkString " ")+" "+e.toString+")"		
	}
	
	abstract class def_cps extends CPSNode
	
	case class vardef_cps(v:var_cps,a:aexp_cps) extends def_cps with CPSNode{
		override def toString = "(define "+v.toString+" "+a.toString+")"
	}
	
	case class envdef_cps(name:String,l:List[String]) extends def_cps with CPSNode{
		override def toString = "(define-env "+name+" ("+(l mkString " ")+"))"
	}
	
	case class labeldef_cps(name:String,l:lambda_cps) extends def_cps with CPSNode{
		override def toString = "(define-label "+name+" "+l.toString+")"
	}
	
	abstract class aexp_cps extends CPSNode{
		override def isAtomic:Boolean = true
	}
	
	case class lambda_label_cps(label:String) extends aexp_cps with CPSNode{
		override def toString = "(lambda-label "+label+")"
	}
	
	case class void_cps extends aexp_cps with CPSNode{
		override def toString = "(void)"	
	}
	
	case class var_cps(s:String) extends aexp_cps with CPSNode{
		override def toString = s		
	}
	
	case class num_cps(i:String) extends aexp_cps with CPSNode{
		override def toString = i 
	}
	
	case class string_cps(s:String) extends aexp_cps with CPSNode{
		override def toString = "\""+s+"\""
	}
	
	case class lambda_cps(l:List[aexp_cps],e:CPSNode) extends aexp_cps with CPSNode{
		override def toString = "(lambda ("+(l mkString " ")+") "+e.toString+")"
	}
	
	case class set_cps(l:List[aexp_cps]) extends aexp_cps with CPSNode{
				override def toString = "(set "+(l mkString " ")+")"
	}
	
	case class tuple_cps(l:List[aexp_cps]) extends aexp_cps with CPSNode{
				override def toString = "(tuple "+(l mkString " ")+")"
	}
	
	case class py_list_cps(l:List[aexp_cps]) extends aexp_cps with CPSNode{
				override def toString = "(py-list* "+(l mkString " ")+")"
	}
	
	case class dict_cps(l:List[(aexp_cps,aexp_cps)]) extends aexp_cps with CPSNode{
				override def toString = "(dict "+((l map ((x) => "("+x._1.toString+" "+x._2.toString+")")) mkString " ")+")"
	}
	
	case class make_cell_cps(a:aexp_cps) extends aexp_cps with CPSNode{
				override def toString = "(make-cell "+a.toString+")"
	}
	
	case class get_cell_cps(a:aexp_cps) extends aexp_cps with CPSNode{
				override def toString = "(get-cell "+a.toString+")"
	}
	
	case class make_closure_cps(a:aexp_cps,b:aexp_cps) extends aexp_cps with CPSNode{
				override def toString = "(make-closure "+a.toString+" "+b.toString+")" 
	}
	
	case class make_env_cps(name:String, l:scala.collection.immutable.HashMap[aexp_cps,aexp_cps]) extends aexp_cps with CPSNode{
				override def toString = "(make-env "+" "+name+" "+((l map ((x) => "("+x._1+" "+x._2+")")) mkString " ")+")" 
	}
	
	case class env_ref_cps(name:String, field_name:String, a:aexp_cps) extends aexp_cps with CPSNode{
		
				override def toString = "(env-ref "+name+" "+field_name+" "+a.toString+")" 
	}
	
	abstract class cexp_cps extends CPSNode
	
		case class error_cps(e:aexp_cps, k:aexp_cps) extends cexp_cps with CPSNode{
		var str =  "(error "+e.toString+" "+k.toString+")"
		//if(e.isInstanceOf[string_cps])
			//str =  "(error \""+e.toString+" "+k.toString+"\")"
		override def toString = str
	}
	
	
	
	case class if_cps(a:aexp_cps,b:cexp_cps,c:cexp_cps) extends cexp_cps with CPSNode{
		override def toString = "(if "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class set_then_bang_cps(v:var_cps,e1:aexp_cps,e2:cexp_cps) extends cexp_cps with CPSNode{
		override def toString ="(set-then! "+v.toString+" "+e1.toString+" "+e2.toString+")"
	}

	case class func_app_cps(l:List[aexp_cps]) extends cexp_cps with CPSNode{
		override def toString ="(app* "+(l mkString " ")+")"
	}

	case class triop_exp_cps(op:triop_cps,a:aexp_cps,b:aexp_cps,c:aexp_cps,d:aexp_cps) extends cexp_cps with CPSNode{
		override def toString = "((cps "+op.toString+") "+a.toString+" "+b.toString+" "+c.toString+" "+d.toString+")"
	}
	
	case class binop_exp_cps(op:binop_cps,a:aexp_cps,b:aexp_cps,c:aexp_cps) extends cexp_cps with CPSNode{
		override def toString = "((cps "+op.toString+") "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class unop_exp_cps(op:unop_cps,a:aexp_cps,b:aexp_cps) extends cexp_cps with CPSNode{
		var str = "((cps "+op.toString+") "+a.toString+" "+b.toString+")"
		if(op.op.equals("py-print") && a.isInstanceOf[string_cps])
			str = "((cps "+op.toString+") \""+a.toString+" "+b.toString+"\")"	
		override def toString = str
	}

	case class lib_fun_cps(f:var_cps,l:List[aexp_cps]) extends cexp_cps with CPSNode{
		override def toString = "("+f.toString+" "+(l mkString " ")+")"
	}
	
	case class set_cell_bang_cps(a:aexp_cps,b:aexp_cps,c:cexp_cps) extends cexp_cps with CPSNode{
		override def toString = "(set-cell! "+a.toString+" "+b.toString+" "+c.toString+")"
	}
	
	case class triop_cps(op:String) extends CPSNode{
		override def toString = op
	}
	
	case class binop_cps(op:String) extends CPSNode{
		override def toString = op
	}
	
	case class unop_cps(op:String) extends CPSNode{
		override def toString = op
	}
	
	case class cps_fun_cps(op:String) extends CPSNode{
		override def toString = op
	}
	
	var counter = 13
	def gensym(s:String):String = {
		counter = counter + 1
		return s+counter
	}
	
	def cps_atom(a:exp_lir):aexp_cps = {
		a match{
			case l:lambda_lir => {val k = gensym("$k")
								  val var_list = (var_cps(k)::(l.l map ((x) => cps_atom(x).asInstanceOf[var_cps])).reverse).reverse
									lambda_cps(var_list,cps_transform_q(l.e,var_cps(k)))			
			}
			case v:void_lir => void_cps()
			case v:var_lir => {
				if(v.s == "call/ec")
					lambda_cps(List(var_cps("f"),var_cps("cc")), func_app_cps(List(var_cps("f"), lambda_cps(List(var_cps("x"),var_cps("_")), 
						func_app_cps(List(var_cps("cc"),var_cps("x")))), var_cps("cc"))))
				else
					var_cps(v.s)
			}
			case n:num_lir => num_cps(n.i)
			case s:string_lir => string_cps(s.s)
			case s:set_lir => {
				set_cps(s.l map cps_atom)
			}
			case d:dict_lir => {
				dict_cps(d.l map ((x) => (cps_atom(x._1),cps_atom(x._2))))
			}
			case t:tuple_lir => {
				tuple_cps(t.l map cps_atom)
			}
			case p:py_list_lir => {
				py_list_cps(p.l map cps_atom)
			}
			
		/*	case c:call_ec_lir => {
				val exp = cps_atom(c.e)
				lambda_cps(List(var_cps("f"),var_cps("cc")), func_app_cps(List(var_cps("f"), lambda_cps(List(var_cps("x"),var_cps("_")), 
						func_app_cps(List(var_cps("cc"),var_cps("x")))), var_cps("cc"))))
			}*/
			
			case _ => throw new Exception("cps_atom error")
		}
	} 
	
	def cps_transform_def(v:vardef_lir):vardef_cps = {
			vardef_cps(var_cps(v.v.s), cps_atom(v.a))
	}
	
	def cps_transform_program(e:LirNode):CPSNode = {
			e match {
				case p:program_lir => program_cps(p.l map cps_transform_def, cps_transform_q(p.e, var_cps("$halt")))
				case _ => throw new Exception("incorrect input")
			}
	
	}
	
	def cps_transform_k(e:exp_lir, k:(aexp_cps => cexp_cps)):cexp_cps ={
		if(e.isAtomic)
			return k(cps_atom(e))
		e match {
			case b:begin_lir => { 
				if(b.l.length == 1)
					return cps_transform_k(b.l(0),k)
				return cps_transform_k(b.l(0), ((_) => cps_transform_k(begin_lir(b.l.drop(1)),k)))
			}
			case f:func_app_lir => {
				cps_transform_q(e, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case b:binop_exp_lir => {
				cps_transform_q(e, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case u:unop_exp_lir => {
				cps_transform_q(e, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case c:call_ec_lir => {
				cps_transform_q(e, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case t:triop_exp_lir => {
				cps_transform_q(e, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case s:set_bang_lir => {
				cps_transform_q(s, ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k))
			}
			
			case i:if_lir => {
				val q = ((kont:(var_cps => cexp_cps)) => {val sym = var_cps(gensym("rv"))
											lambda_cps(List(sym), kont(sym))
											})(k)
				cps_transform_k(i.a, ((c) => if_cps(c, cps_transform_q(i.b,q), cps_transform_q(i.c,q))))
			}
			
			case _ => throw new Exception("not matched in cps-transform-k")
		}
	}
	
	def cps_transform_q(e:exp_lir, q:aexp_cps):cexp_cps = {
		if(e.isAtomic)
			return func_app_cps(List(q, cps_atom(e)))
		e match {
			case b:begin_lir => { 
				if(b.l.length == 1)
					return cps_transform_q(b.l(0),q)
				return cps_transform_k(b.l(0), ((_) => cps_transform_q(begin_lir(b.l.drop(1)),q)))
			}
			case i:if_lir => {
				cps_transform_k(i.a, ((c) => if_cps(c, cps_transform_q(i.b,q), cps_transform_q(i.c,q))))
			}
			
			case f:func_app_lir => {				
				//cps_transform_k(f.l(0), lambda_cps(List(var_cps("$f")), cps_transform_k_star(f.l.tail,  lambda_cps(List(var_cps("$f"))
				cps_transform_k(f.l(0), (($f:aexp_cps) => cps_transform_k_star(f.l.tail, (($es) => {
																						var fname:aexp_cps = 
																						if(primitive_operation_huh($f))
																							var_cps("(cps "+$f.toString+")")
																						else
																							$f
																						func_app_cps(fname::$es++List(q))
																					}))))
			}
			
			case b:binop_exp_lir => {
				cps_transform_k(var_lir(b.op.op), (($f:aexp_cps) => cps_transform_k_star(List(b.a,b.b), (($es) => {
																						var fname:aexp_cps = 
																						if(primitive_operation_huh($f))
																							$f//var_cps("(cps "+$f.toString+")")
																						else
																							$f
																						binop_exp_cps(binop_cps(fname.toString),$es(0),$es(1),q)
																					}))))
			
			}
			
			case c:call_ec_lir => {
	//			val call_ec_obj = lambda_cps(List(var_cps("f"),var_cps("cc")), func_app_cps(List(var_cps("f"), lambda_cps(List(var_cps("x"),var_cps("_")), 
		//				func_app_cps(List(var_cps("cc"),var_cps("x")))), var_cps("cc"))))
				
				cps_transform_k(var_lir("call/ec"), (($f:aexp_cps) => cps_transform_k_star(List(c.e), (($es) => {
																						var fname:aexp_cps = 
																						if(primitive_operation_huh($f))
																							var_cps("(cps "+$f.toString+")")
																						else
																							$f
																						func_app_cps(fname::$es++List(q))
																					}))))
			
			}
			
			case u:unop_exp_lir => {
				cps_transform_k(var_lir(u.op.op), (($f:aexp_cps) => cps_transform_k_star(List(u.a), (($es) => {
																						var fname:aexp_cps = 
																						if(primitive_operation_huh($f))
																							$f//var_cps("(cps "+$f.toString+")")
																						else
																							$f
																						unop_exp_cps(unop_cps(fname.toString),$es(0),q)
																					}))))
			
			}
			
			case t:triop_exp_lir => {
				cps_transform_k(var_lir(t.op.op), (($f:aexp_cps) => cps_transform_k_star(List(t.a,t.b,t.c), (($es) => {
																						var fname:aexp_cps = 
																						if(primitive_operation_huh($f))
																							$f//var_cps("(cps "+$f.toString+")")
																						else
																							$f
																						triop_exp_cps(triop_cps(fname.toString),$es(0),$es(1),$es(2),q)
																					}))))
			
			}
			
			case s:set_bang_lir =>{
				cps_transform_k(s.e, ((aexp) => set_then_bang_cps(var_cps(s.v.s), aexp, func_app_cps(List(q,void_cps())))))
			
			}
			
			case e:error_lir => error_cps(cps_atom(e.e),q)
			
			
			case _ => throw new Exception("not matched in cps-transform-q")
								
		}
	}
	
	def primitive_operation_huh(v:aexp_cps):Boolean ={
		v match{
			case v1:var_cps => {
				val s = v1.s
				(s == "+" || s== "*"|| s == "/" || s== "-" || s == "py-print" ||
				s == "<" || s== ">" || s == "equal?" || s == ">=" || s == "<=" || s == "not-equal?" || s == "in?" || 
				s == "not-in?" || s== "eq?" || s == "not-eq?" ||  s == "<<" || s == ">>" ||
				s == "quotient" || s == "modulo" || s == "expt" || s == "assert2" ||
                s == "bitwise-and" || s == "bitwise-or" || s == "bitwise-xor" || s == "py-list-ref" ||
                s == "py-list-remove!" || s == "tuple-ref" || s == "tuple-set!" || s == "dict=ref" || 
                s == "dict-remove!" || s == "bitwise-not" || s == "integer?" || s == "string?" ||
                s == "tuple?" || s == "dict?" || s == "py-list?" || s == "set?" || s == "assert1" ||
                s == "py-print" || s == "not" || s == "py-list-set!" || s == "dict-set!" ||  s == "dict-set!")
			}
			case _ => false
		}
	}
	
	def cps_transform_k_star(e:List[exp_lir],k:(List[aexp_cps] => cexp_cps)):cexp_cps ={
		if(e.length == 0){
			k(List())
		}
		else{
			cps_transform_k(e(0), ((hd) => cps_transform_k_star(e.drop(1), ((tl) => k(hd::tl)))))
		}
	}
	
	
}


object CPSToC{
	import CPS._
	
	
	
	var mutables_set:Set[String] = Set()
	var globals_set:Set[String] = Set()
	var global_defines:List[envdef_cps] = List()
	var lifted_lambdas:List[labeldef_cps] = List()
	
	private var environments = scala.collection.immutable.HashMap[Int, List[aexp_cps]]()
	var numEnv = 0
	
	def allocate_environment(fields:List[aexp_cps]):Int = {
		environments = (environments.updated(numEnv,fields))
		numEnv += 1
		return numEnv - 1
	}
	
	def make_global_set(l:List[def_cps]) ={
		l map ((x) => x match{
						case v:vardef_cps =>globals_set = globals_set + v.v.toString
						case _ =>
		})
		globals_set  = globals_set - "return"
	}
	
	def analyze_mutable_variables(e:CPSNode):Unit = {		
		e match {
			case a:aexp_cps => {
				a match {
					case l:lambda_cps => analyze_mutable_variables(l.e)
					case s:set_cps => s.l foreach analyze_mutable_variables
					case d:dict_cps => d.l foreach ((x) => {analyze_mutable_variables(x._1);analyze_mutable_variables(x._2)}) 
					case t:tuple_cps => t.l foreach analyze_mutable_variables
					case p:py_list_cps => p.l foreach analyze_mutable_variables
					case _ =>
				}
			}
			case c:cexp_cps => {
				c match {
					case i:if_cps => List(i.a,i.b,i.c) foreach analyze_mutable_variables
					case e:error_cps => List(e.e,e.k) foreach analyze_mutable_variables
					case s:set_then_bang_cps => {
						mutables_set = mutables_set + (s.v.toString)
						List(s.e1,s.e2) foreach analyze_mutable_variables
					}
					case c:func_app_cps => {
						c.l foreach analyze_mutable_variables
					}
					case t:triop_exp_cps => List(t.a,t.b,t.c,t.d) foreach analyze_mutable_variables
					case b:binop_exp_cps => List(b.a,b.b,b.c) foreach analyze_mutable_variables
					case u:unop_exp_cps => List(u.a,u.b) foreach analyze_mutable_variables
					case l:lib_fun_cps => l.l foreach analyze_mutable_variables
				}
				
			}
			case a => a
		}
	}
	
	def isMutable(a:aexp_cps):Boolean ={
		mutables_set.contains(a.toString) && !globals_set.contains(a.toString)
		
	}
	
	def wrap_mutable_formals(formals:List[aexp_cps],body:CPSNode):CPSNode ={
		if(formals.size == 0)
			return body
		if(isMutable(formals(0))){
			set_then_bang_cps(formals(0).asInstanceOf[var_cps],make_cell_cps(formals(0)),wrap_mutable_formals(formals.drop(1),body).asInstanceOf[cexp_cps])
		}
		else
			wrap_mutable_formals(formals.drop(1),body)
	}
	
	//if the particular variable is a mutable variable and not a global variable, allocate a cell for that. 
	def wrap_mutables(e:CPSNode):CPSNode = {
		e match {
			
				
					case l:lambda_cps => lambda_cps(l.l,wrap_mutable_formals(l.l,l.e))
					case a:var_cps => if(isMutable(a))
										get_cell_cps(a)
									  else
									 	a
					case a:string_cps => if(isMutable(a))
										get_cell_cps(a)
									  else
									 	a					//this is not needed but added just in case
					case i:if_cps => if_cps(wrap_mutables(i.a).asInstanceOf[aexp_cps],
											wrap_mutables(i.b).asInstanceOf[cexp_cps],
											wrap_mutables(i.c).asInstanceOf[cexp_cps])
					case s:set_cps => set_cps(s.l map ((x) => wrap_mutables(x).asInstanceOf[aexp_cps]))
					case s:dict_cps => dict_cps(s.l map ((x) => (wrap_mutables(x._1).asInstanceOf[aexp_cps],wrap_mutables(x._1).asInstanceOf[aexp_cps])))
					case s:tuple_cps => tuple_cps(s.l map ((x) => wrap_mutables(x).asInstanceOf[aexp_cps]))
					case s:py_list_cps => py_list_cps(s.l map ((x) => wrap_mutables(x).asInstanceOf[aexp_cps]))
					case e:error_cps => error_cps(wrap_mutables(e.e).asInstanceOf[aexp_cps],wrap_mutables(e.k).asInstanceOf[aexp_cps])
					
					case s:set_then_bang_cps => {
						if(isMutable(s.v))
								set_cell_bang_cps(s.v,wrap_mutables(s.e1).asInstanceOf[aexp_cps],wrap_mutables(s.e2).asInstanceOf[cexp_cps])
						else
							set_then_bang_cps(s.v,wrap_mutables(s.e1).asInstanceOf[aexp_cps],wrap_mutables(s.e2).asInstanceOf[cexp_cps])
					}
					case c:func_app_cps => {
						func_app_cps(c.l map ((x) => wrap_mutables(x).asInstanceOf[aexp_cps]))
					}
					case t:triop_exp_cps => triop_exp_cps(t.op,
														  wrap_mutables(t.a).asInstanceOf[aexp_cps],
														  wrap_mutables(t.b).asInstanceOf[aexp_cps],
														  wrap_mutables(t.c).asInstanceOf[aexp_cps],
														  wrap_mutables(t.d).asInstanceOf[aexp_cps])
					case b:binop_exp_cps => binop_exp_cps(b.op,
														  wrap_mutables(b.a).asInstanceOf[aexp_cps],
														  wrap_mutables(b.b).asInstanceOf[aexp_cps],
														  wrap_mutables(b.c).asInstanceOf[aexp_cps])
					case u:unop_exp_cps => unop_exp_cps(u.op,
														  wrap_mutables(u.a).asInstanceOf[aexp_cps],
														  wrap_mutables(u.b).asInstanceOf[aexp_cps])
														  
					case l:lib_fun_cps => lib_fun_cps(l.f,l.l map ((x) => wrap_mutables(x).asInstanceOf[aexp_cps]))
					case a => a
					
				
			}
		
		}
	
	
	def mutable_convert(e:CPSNode):CPSNode ={
		e match {
			case p:program_cps => {
				analyze_mutable_variables(p.e)
				make_global_set(p.l)
				program_cps(p.l,wrap_mutables(p.e).asInstanceOf[cexp_cps])
			}
			case _ => throw new Exception("incorrect input")
		}		
	}
	
	def closure_convert(e:CPSNode):CPSNode = {
		e match{
			case p:program_cps => {
				val exp = closure_convert(p.e).asInstanceOf[cexp_cps]
				program_cps(global_defines++p.l,exp)
			}
			case l:lambda_cps =>{
				val $env = gensym("$env_t")
				val params =var_cps("$env")::l.l
				val fv:Set[aexp_cps] = get_free(l).foldRight(Set[aexp_cps]())((a,b) => if(globals_set.contains(a.toString) || a.toString == "$halt")
																						 b
																					   else
																					  	 b+a)  
				val closure_conv_body = closure_convert(l.e)
				global_defines = envdef_cps($env,fv.toList map ((x) => x.toString))::global_defines
				
				//val id = allocate_environment(fv.toList)
				val env = fv.foldRight(scala.collection.immutable.HashMap[aexp_cps,aexp_cps]())((a,b) => {
						b+(a->a)
				})
				val sub = fv.toList.foldRight(scala.collection.immutable.HashMap[aexp_cps,aexp_cps]())((a,b) => b+(a->env_ref_cps($env,"$env",a)))
				
				val body  = (substitute(sub,closure_conv_body))
				make_closure_cps(lambda_cps(params,body),make_env_cps($env,env))
				
			}
			
			case i:if_cps => if_cps(closure_convert(i.a).asInstanceOf[aexp_cps],
									closure_convert(i.b).asInstanceOf[cexp_cps],
									closure_convert(i.c).asInstanceOf[cexp_cps])
			case s:set_cps => set_cps(s.l map ((x) => closure_convert(x).asInstanceOf[aexp_cps]))			
			case s:dict_cps => dict_cps(s.l map ((x) => (closure_convert(x._1).asInstanceOf[aexp_cps],closure_convert(x._2).asInstanceOf[aexp_cps])))				
			case s:tuple_cps => tuple_cps(s.l map ((x) => closure_convert(x).asInstanceOf[aexp_cps]))	
			case s:py_list_cps => py_list_cps(s.l map ((x) => closure_convert(x).asInstanceOf[aexp_cps]))
			case e:error_cps => error_cps(closure_convert(e.e).asInstanceOf[aexp_cps],closure_convert(e.k).asInstanceOf[aexp_cps])
			case i:set_then_bang_cps => set_then_bang_cps(i.v,
														  closure_convert(i.e1).asInstanceOf[aexp_cps],
														  closure_convert(i.e2).asInstanceOf[cexp_cps])
			case m:make_cell_cps => make_cell_cps(closure_convert(m.a).asInstanceOf[aexp_cps])
			case s:get_cell_cps => get_cell_cps(closure_convert(s.a).asInstanceOf[aexp_cps])
			case i:set_cell_bang_cps => set_cell_bang_cps(closure_convert(i.a).asInstanceOf[aexp_cps],
														  closure_convert(i.b).asInstanceOf[aexp_cps],
														  closure_convert(i.c).asInstanceOf[cexp_cps])
			case m:make_closure_cps => m//make_closure_cps(closure_convert(m.a).asInstanceOf[aexp_cps],
												//		  closure_convert(m.b).asInstanceOf[aexp_cps])
			case m:make_env_cps => m//make_env_cps(m.name, (m.l map ((x) => (closure_convert(x._1).asInstanceOf[aexp_cps],closure_convert(x._2).asInstanceOf[aexp_cps]))))
			case r:env_ref_cps => r//env_ref_cps(r.name,r.field_name,closure_convert(r.a).asInstanceOf[aexp_cps])
			case f:func_app_cps => func_app_cps(f.l map ((x) => closure_convert(x).asInstanceOf[aexp_cps]))
			case i:triop_exp_cps => triop_exp_cps(i.op, closure_convert(i.a).asInstanceOf[aexp_cps],
													closure_convert(i.b).asInstanceOf[aexp_cps],
													closure_convert(i.c).asInstanceOf[aexp_cps],
													closure_convert(i.d).asInstanceOf[aexp_cps])
			case i:binop_exp_cps => binop_exp_cps(i.op, closure_convert(i.a).asInstanceOf[aexp_cps],
													closure_convert(i.b).asInstanceOf[aexp_cps],
													closure_convert(i.c).asInstanceOf[aexp_cps])
			case i:unop_exp_cps => unop_exp_cps(i.op, closure_convert(i.a).asInstanceOf[aexp_cps],
													closure_convert(i.b).asInstanceOf[aexp_cps])
			case l:lib_fun_cps => lib_fun_cps(l.f,l.l map ((x) => closure_convert(x).asInstanceOf[aexp_cps]))
			case v:void_cps => v  
			case a => a
			
		}
	}
	
	def removeKeys(vars:List[aexp_cps],sub:scala.collection.immutable.HashMap[aexp_cps,aexp_cps]):scala.collection.immutable.HashMap[aexp_cps,aexp_cps] = {
		vars.foldRight(sub)((a:aexp_cps,b:scala.collection.immutable.HashMap[aexp_cps,aexp_cps]) => {
									  if(!b.contains(a))
										b
									  else
										b - a})	
	}
	
	def substitute(sub:scala.collection.immutable.HashMap[aexp_cps,aexp_cps],exp:CPSNode):CPSNode = {
		exp match{
			case v:var_cps => {
				if(sub.contains(v))			
					sub(v)
				else
				 	v
			}
			case l:lambda_cps => lambda_cps(l.l,substitute(removeKeys(l.l,sub),l.e))
			case i:if_cps => if_cps(substitute(sub,i.a).asInstanceOf[aexp_cps],
									substitute(sub,i.b).asInstanceOf[cexp_cps],
									substitute(sub,i.c).asInstanceOf[cexp_cps])
			case s:set_cps => set_cps(s.l map ((x) => substitute(sub,x).asInstanceOf[aexp_cps]))			
			case s:dict_cps => dict_cps(s.l map ((x) => (substitute(sub,x._1).asInstanceOf[aexp_cps],substitute(sub,x._2).asInstanceOf[aexp_cps])))				
			case s:tuple_cps => tuple_cps(s.l map ((x) => substitute(sub,x).asInstanceOf[aexp_cps]))	
			case s:py_list_cps => py_list_cps(s.l map ((x) => substitute(sub,x).asInstanceOf[aexp_cps]))
			case e:error_cps => error_cps(substitute(sub,e.e).asInstanceOf[aexp_cps],substitute(sub,e.k).asInstanceOf[aexp_cps])
			case i:set_then_bang_cps => set_then_bang_cps(i.v,
														  substitute(sub,i.e1).asInstanceOf[aexp_cps],
														  substitute(sub,i.e2).asInstanceOf[cexp_cps])
			case m:make_cell_cps => make_cell_cps(substitute(sub,m.a).asInstanceOf[aexp_cps])
			case s:get_cell_cps => get_cell_cps(substitute(sub,s.a).asInstanceOf[aexp_cps])
			case i:set_cell_bang_cps => set_cell_bang_cps(substitute(sub,i.a).asInstanceOf[aexp_cps],
														  substitute(sub,i.b).asInstanceOf[aexp_cps],
														  substitute(sub,i.c).asInstanceOf[cexp_cps])
			case m:make_closure_cps => make_closure_cps(substitute(sub,m.a).asInstanceOf[aexp_cps],
														  substitute(sub,m.b).asInstanceOf[aexp_cps])
			case m:make_env_cps => make_env_cps(m.name,(m.l map ((x) => (x._1,substitute(sub,x._2).asInstanceOf[aexp_cps]))))
			case r:env_ref_cps => r//env_ref_cps(r.name,r.field_name,substitute(sub,r.a).asInstanceOf[aexp_cps])
			case f:func_app_cps => func_app_cps(f.l map ((x) => substitute(sub,x).asInstanceOf[aexp_cps]))
			case i:triop_exp_cps => triop_exp_cps(i.op, substitute(sub,i.a).asInstanceOf[aexp_cps],
													substitute(sub,i.b).asInstanceOf[aexp_cps],
													substitute(sub,i.c).asInstanceOf[aexp_cps],
													substitute(sub,i.d).asInstanceOf[aexp_cps])
			case i:binop_exp_cps => binop_exp_cps(i.op, substitute(sub,i.a).asInstanceOf[aexp_cps],
													substitute(sub,i.b).asInstanceOf[aexp_cps],
													substitute(sub,i.c).asInstanceOf[aexp_cps])
			case i:unop_exp_cps => unop_exp_cps(i.op, substitute(sub,i.a).asInstanceOf[aexp_cps],
													substitute(sub,i.b).asInstanceOf[aexp_cps])
			case l:lib_fun_cps => lib_fun_cps(l.f,l.l map ((x) => substitute(sub,x).asInstanceOf[aexp_cps]))
			case a => a
		}
	}
	
	def get_free(e:CPSNode):Set[aexp_cps] = {
		e match {
			case v:var_cps => {
				if(v.toString == "return") {
					var str = "ab"
						str
				}
					
				Set(v)
			}
			case l:lambda_cps => {
				val free = get_free(l.e) -- l.l
				free
			}
			case i:if_cps => get_free(i.a)++get_free(i.b)++get_free(i.c)
			case s:set_cps => (s.l map get_free).foldRight(Set[aexp_cps]())((a,b) => b++a)			
			case s:dict_cps => (s.l map ((x) => get_free(x._1)++get_free(x._2))).foldRight(Set[aexp_cps]())((a,b) => b++a)			
			case s:tuple_cps => (s.l map get_free).foldRight(Set[aexp_cps]())((a,b) => b++a)
			case s:py_list_cps => (s.l map get_free).foldRight(Set[aexp_cps]())((a,b) => b++a)
			case e:error_cps => get_free(e.e)
			case s:set_then_bang_cps => get_free(s.v)++get_free(s.e1)++get_free(s.e2)
			case m:make_cell_cps => get_free(m.a)
			case s:get_cell_cps => get_free(s.a)
			case i:set_cell_bang_cps => get_free(i.a)++get_free(i.b)++get_free(i.c)
			case m:make_closure_cps => get_free(m.a)++get_free(m.b)
			case m:make_env_cps => (m.l map ((x) => get_free(x._2))).foldRight(Set[aexp_cps]())((a,b) => b++a)
			case r:env_ref_cps => get_free(r.a)
			case f:func_app_cps => (f.l map get_free).foldRight(Set[aexp_cps]())((a,b) => b++a)
			case t:triop_exp_cps => get_free(t.a)++get_free(t.b)++get_free(t.c)++get_free(t.d)
			case t:binop_exp_cps => get_free(t.a)++get_free(t.b)++get_free(t.c)
			case t:unop_exp_cps => get_free(t.a)++get_free(t.b)
			case l:lib_fun_cps => get_free(l.f)++(l.l map get_free).foldRight(Set[aexp_cps]())((a,b) => b++a)
			case _ => Set()
		}
	}
	
	def lambda_lift(e:CPSNode):CPSNode = {
		e match{
			case p:program_cps =>{
				val lifted_exp = lambda_lift(p.e)
				program_cps(lifted_lambdas++p.l, lifted_exp.asInstanceOf[cexp_cps]) 
			}
			case l:lambda_cps =>{
				val new_label = gensym("$lambda")
				lifted_lambdas = labeldef_cps(new_label,lambda_cps(l.l,lambda_lift(l.e)))::lifted_lambdas
				lambda_label_cps(new_label)
			}
			
			case i:if_cps => if_cps(lambda_lift(i.a).asInstanceOf[aexp_cps],
									lambda_lift(i.b).asInstanceOf[cexp_cps],
									lambda_lift(i.c).asInstanceOf[cexp_cps])
			case s:set_cps => set_cps(s.l map ((x) => lambda_lift(x).asInstanceOf[aexp_cps]))			
			case s:dict_cps => dict_cps(s.l map ((x) => (lambda_lift(x._1).asInstanceOf[aexp_cps],lambda_lift(x._2).asInstanceOf[aexp_cps])))				
			case s:tuple_cps => tuple_cps(s.l map ((x) => lambda_lift(x).asInstanceOf[aexp_cps]))	
			case s:py_list_cps => py_list_cps(s.l map ((x) => lambda_lift(x).asInstanceOf[aexp_cps]))
			case e:error_cps => error_cps(lambda_lift(e.e).asInstanceOf[aexp_cps],lambda_lift(e.k).asInstanceOf[aexp_cps])
			case i:set_then_bang_cps => set_then_bang_cps(i.v,
														  lambda_lift(i.e1).asInstanceOf[aexp_cps],
														  lambda_lift(i.e2).asInstanceOf[cexp_cps])
			case m:make_cell_cps => make_cell_cps(lambda_lift(m.a).asInstanceOf[aexp_cps])
			case s:get_cell_cps => get_cell_cps(lambda_lift(s.a).asInstanceOf[aexp_cps])
			case i:set_cell_bang_cps => set_cell_bang_cps(lambda_lift(i.a).asInstanceOf[aexp_cps],
														  lambda_lift(i.b).asInstanceOf[aexp_cps],
														  lambda_lift(i.c).asInstanceOf[cexp_cps])
			case m:make_closure_cps => make_closure_cps(lambda_lift(m.a).asInstanceOf[aexp_cps],
														  lambda_lift(m.b).asInstanceOf[aexp_cps])
			case m:make_env_cps => make_env_cps(m.name,(m.l map ((x) => (lambda_lift(x._1).asInstanceOf[aexp_cps],lambda_lift(x._2).asInstanceOf[aexp_cps]))))
			case r:env_ref_cps => env_ref_cps(r.name,r.field_name,lambda_lift(r.a).asInstanceOf[aexp_cps])
			case f:func_app_cps => func_app_cps(f.l map ((x) => lambda_lift(x).asInstanceOf[aexp_cps]))
			case i:triop_exp_cps => triop_exp_cps(i.op, lambda_lift(i.a).asInstanceOf[aexp_cps],
													lambda_lift(i.b).asInstanceOf[aexp_cps],
													lambda_lift(i.c).asInstanceOf[aexp_cps],
													lambda_lift(i.d).asInstanceOf[aexp_cps])
			case i:binop_exp_cps => binop_exp_cps(i.op, lambda_lift(i.a).asInstanceOf[aexp_cps],
													lambda_lift(i.b).asInstanceOf[aexp_cps],
													lambda_lift(i.c).asInstanceOf[aexp_cps])
			case i:unop_exp_cps => unop_exp_cps(i.op, lambda_lift(i.a).asInstanceOf[aexp_cps],
													lambda_lift(i.b).asInstanceOf[aexp_cps])
			case l:lib_fun_cps => lib_fun_cps(l.f,l.l map ((x) => lambda_lift(x).asInstanceOf[aexp_cps]))
			case a => a
		}
	}
	
	
	
}


object HirToLirToCPS {
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
	val input = scala.io.Source.fromFile(args(0)).mkString("")
	//PyParser.print(input) 
	val hir_parsed = HIRParser.parseHir(input)
	//println(hir_parsed)
	val lir_converted = LIR.convert(hir_parsed)
	//println(lir_converted)
	val cps_converted = CPS.cps_transform_program(lir_converted)
	val mutable_converted = CPSToC.mutable_convert(cps_converted)
	val closure_converted = CPSToC.closure_convert(mutable_converted)
	val lambda_lifted = CPSToC.lambda_lift(closure_converted)
	println(closure_converted)
	println(lambda_lifted)    
  }

}