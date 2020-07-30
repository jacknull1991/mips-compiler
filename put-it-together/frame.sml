signature FRAME =
sig
    type frame
    type access
    datatype register = Reg of string
    val RV: Temp.temp (* function return value register *)
    val RA: Temp.temp (* function return address register *)
    val SP: Temp.temp (* stack pointer register *)
    val FP: Temp.temp (* frame pointer register *)
    val ZERO: Temp.temp (* ZERO register *)
    val specialregs: Temp.temp list
    val callersaves: Temp.temp list (* list of caller save registers *)
    val calleesaves: Temp.temp list (* list of callee save registers *)
    val argregs: Temp.temp list (* list of function arguments registers *)
    val registers: register list (* list of all register names *)
    val tempMap: register Temp.Table.table (* register to "color" map *)
    val registerName: Temp.temp -> string (* return a temporary name of a given register *)
    val getReg: Temp.temp -> register (* return register name of a given register *)
    val colorList: Temp.temp list (* list of available registers for register allocation *)			     
    val wordsize: int (* word size *)
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

structure MipsFrame:> FRAME =
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
(* ZERO *)
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
fun getReg (temp) =
    (case Temp.Table.look(tempMap, temp) of
	 SOME(reg) => reg
       | NONE => Reg(Temp.makestring(temp)))
	
val registers = map getReg (argregs @ callersaves @ calleesaves)

val colorList = callersaves @ calleesaves @ argregs		    
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

fun string (label, literal) = Symbol.name(label) ^ ": .ascii \"" ^ literal ^ "\"\n"

(* hepler: create a sequence of exp *)
fun listToSeq ([]) = T.EXP(T.CONST 0)
  | listToSeq ([exp]) = exp
  | listToSeq (exp::exps) = T.SEQ(exp, (listToSeq(exps)))
										 
(* TODO: save registers on stack only when allocation fails *)
fun procEntryExit1 (frame, body) = body
    (*let
	val regsToSave = RA :: calleesaves
	val tempRegisters = map (fn _ => Temp.newtemp()) regsToSave
	fun moveRegs (from, to) = T.MOVE(T.TEMP to, T.TEMP from)					    
	val saveRegs = listToSeq(ListPair.mapEq moveRegs (tempRegisters, regsToSave))
	val restoreRegs = listToSeq(ListPair.mapEq moveRegs (regsToSave, tempRegisters))

	val newbody = listToSeq([saveRegs, body, restoreRegs])
	val params = formals frame

	fun moveArguments (arg, access) =
	    T.MOVE((exp access (T.TEMP FP)), T.TEMP(arg))
    in
	(case params of
	     [] => newbody
	   | _ => T.SEQ(listToSeq(ListPair.map moveArguments (argregs, params)), newbody)
	)
    end*)

fun procEntryExit2 (frame, body) =
    body @ [A.OPER{assem="",
		   src=[ZERO, RA, SP]@calleesaves,
		   dst=[],
		   jump=SOME[]}]

(* create prologue and epilogue instructions *)	       
fun procEntryExit3 ({name, formals, offset}: frame, body: A.instr list) =
    let
	(* frame size = offset + #argregs + ra *)
	val framesize = !offset + (List.length(argregs) * wordsize) + wordsize
    in
	(* save $fp, save $ra, setup new $fp *) 
	{prolog="\n" ^ Symbol.name(name) ^ ":\n"
		^ "sub $sp, $sp, " ^ Int.toString framesize ^ "\n"
		^ "sw $fp, 4($sp)\n"
		^ "sw $ra, 8($sp)\n"
		^ "move $fp, $sp\n",
	 body=body,
	 (* restore $ra, restore $fp *)
	 epilog="move $sp, $fp\n"
		^ "lw $fp, 4($sp)\n"
		^ "lw $ra, 8($sp)\n"
		^ "addi $sp, $sp, " ^ Int.toString framesize ^ "\n"
		^ "jr $ra\n"
		^ "\n"}
    end			      
end
    
