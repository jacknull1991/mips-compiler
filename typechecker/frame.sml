signature FRAME =
sig
    type frame
    type access
    (* type register = string *)
    val RV: Temp.temp
    val FP: Temp.temp
    (* val registers: register list
    val tempMap: register Temp.Table.table *)
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
								       
end

structure MipsFrame: FRAME =
struct

structure T = Tree

datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list, offset: int ref}
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
(* wordsize = 4 bytes *)		 
val wordsize = 4
		     
fun newFrame ({name, formals}) =
    let
	val offset = ref 0
	(* allocate function arguments in memory or register *)			 
	fun allocateVar([]) = []
	  | allocateVar(escape::bools) =
	    (offset := !offset + wordsize; InFrame(!offset)::allocateVar(bools))
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
    		      
end
    
