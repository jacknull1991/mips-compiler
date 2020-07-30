signature FRAME =
sig
    type frame
    type access
    type register
    val RV: Temp.temp
    val RA: Temp.temp
    val SP: Temp.temp		
    val FP: Temp.temp
    val ZERO: Temp.temp		  
    val specialregs: Temp.temp list			       
    val callersaves: Temp.temp list
    val calleesaves: Temp.temp list
    val argregs: Temp.temp list			   
    (* val registers: register list *)
    val tempMap: register Temp.Table.table
    val registerName: Temp.temp -> string				       
    val wordsize: int
    val exp: access -> Tree.exp -> Tree.exp				       
    val externalCall: string * Tree.exp list -> Tree.exp						    

    val newFrame: {name: Temp.label, formals: bool list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    val string: Tree.label * string -> string
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
					       
    val procEntryExit1: frame * Tree.stm -> Tree.stm
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list
    val procEntryExit3: frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}

end

structure MipsFrame: FRAME =
struct

structure T = Tree
structure A = Assem		  

datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list, offset: int ref}
datatype register = Reg of string			       
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string					   
		 
(* frame pointer register *)		 
val FP = Temp.newtemp()
(* stack pointer register *)
val SP = Temp.newtemp()
(* return value *)
val RV = Temp.newtemp()
(* return address *)
val RA = Temp.newtemp()
(* ZERO ???*)
val ZERO = Temp.newtemp()		       

(* named registers *)
(* caller save *)		    
val t0 = Temp.newtemp()
val t1 = Temp.newtemp()
val t2 = Temp.newtemp()
val t3 = Temp.newtemp()
val t4 = Temp.newtemp()
val t5 = Temp.newtemp()
val t6 = Temp.newtemp()
val t7 = Temp.newtemp()
val t8 = Temp.newtemp()
val t9 = Temp.newtemp()		     

(* callee save *)
val s0 = Temp.newtemp()
val s1 = Temp.newtemp()
val s2 = Temp.newtemp()
val s3 = Temp.newtemp()
val s4 = Temp.newtemp()
val s5 = Temp.newtemp()
val s6 = Temp.newtemp()
val s7 = Temp.newtemp()

(* function arguments *)
val a0 = Temp.newtemp()
val a1 = Temp.newtemp()
val a2 = Temp.newtemp()
val a3 = Temp.newtemp()

val specialregs = [FP, SP, RV, RA, ZERO]		     
val callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
val calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]
val argregs = [a0, a1, a2, a3]

val allregs = [(FP, Reg("$fp")), (SP, Reg("$sp")), (RV, Reg("$rv")), (RA, Reg("$ra")), (ZERO, Reg("$zero")), (t0, Reg("$t0")), (t1, Reg("$t1")), (t2, Reg("$t2")), (t3, Reg("$t3")), (t4, Reg("$t4")), (t5, Reg("$t5")), (t6, Reg("$t6")), (t7, Reg("$t7")), (t8, Reg("$t8")), (t9, Reg("$t9")), (s0, Reg("$s0")), (s1, Reg("$s1")), (s2, Reg("$s2")), (s3, Reg("$s3")), (s4, Reg("$s4")), (s5, Reg("$s5")), (s6, Reg("$s6")), (s7, Reg("$s7")), (a0, Reg("$a0")), (a1, Reg("$a1")), (a2, Reg("$a2")), (a3, Reg("$a3"))] 
val tempMap = foldr (fn ((temp, entry), table) =>
			Temp.Table.enter(table, temp, entry)) Temp.Table.empty allregs
		    
(* wordsize = 4 bytes *)		 
val wordsize = 4

fun registerName (temp) =
    case Temp.Table.look(tempMap, temp) of
	SOME(Reg(name)) => name
      | NONE => ("$" ^ Temp.makestring(temp))		    
		   
fun newFrame ({name, formals}) =
    let
	val offset = ref 0
	(* allocate function arguments in memory or register *)			 
	fun allocateVar([]) = []
	  | allocateVar(escape::bools) =
	    if escape then
		(offset := !offset + wordsize; InFrame(!offset)::allocateVar(bools))
	    else
		InReg(Temp.newtemp())::allocateVar(bools)
    in
	{name=name, formals=allocateVar(formals), offset=offset}
    end	

fun name ({name, formals, offset}) = name

fun formals ({name, formals, offset}) = formals
					   
fun allocLocal ({name, formals, offset}) escape =
    if escape then
	(offset := !offset + wordsize; InFrame(!offset))
    else
	InReg(Temp.newtemp())

fun exp (InFrame(k)) fp: T.exp = T.MEM(T.BINOP(T.PLUS, fp, T.CONST(k)))
  | exp (InReg(k)) fp: T.exp = T.TEMP(k)

fun externalCall (f, args) = T.CALL(T.NAME(Temp.namedlabel(f)), args)

(* used later *)				   
fun string (label, literal) = Symbol.name(label) ^ ": .ascii \"" ^ literal ^ "\"\n"

(* dummy implementation *)										 
fun procEntryExit1 (frame, body) = body

fun procEntryExit2 (frame, body) =
    body @ [A.OPER{assem="",
		   src=[ZERO, RA, SP]@calleesaves,
		   dst=[],
		   jump=SOME[]}]

fun procEntryExit3 ({name, formals, offset}: frame, body: A.instr list) =
    {prolog="PROCEDURE " ^ Symbol.name name ^ "\n",
     body=body,
     epilog="END " ^ Symbol.name name ^ "\n"}
    		      
end
    
