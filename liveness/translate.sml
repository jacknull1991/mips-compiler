(* dummy Translate module, used in later phase, chap 7 *)
signature TRANSLATE =
sig
    type exp
    type level
    type access

    val outermost: level
    val newLevel: {parent:level, name: Temp.label, formals:bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
    val seq: Tree.stm list -> Tree.stm				  

    val unEx: exp -> Tree.exp
    val unNx: exp -> Tree.stm
    val unCx: exp -> (Temp.label * Temp.label) -> Tree.stm

    val intExp: int -> exp
    val stringExp: string -> exp
    val nilExp: unit -> exp
    val OpExp: (exp * Absyn.oper * exp) -> exp
    val seqExp: (exp list) -> exp
    val assignExp: (exp * exp) -> exp
    val letExp: (exp list * exp) -> exp					
    val ifExp: (exp * exp) -> exp
    val ifElseExp: (exp * exp * exp) -> exp
    val whileExp: (exp * exp) -> exp
    val forExp: (exp * exp * exp * exp) -> exp
    val breakExp: Tree.label -> exp
    val callExp: (level * Tree.label * exp list) -> exp
    val recordExp: {length: int, fields: exp list} -> exp 
    val arrayExp: (exp * exp) -> exp				     
    val simpleVar: (access * level) -> exp
    val arrayVar: (exp * exp) -> exp

    val procEntryExit: {level: level, body: exp} -> unit
    val getResult: unit -> MipsFrame.frag list
    val fragref: MipsFrame.frag list ref
		     
end    

structure Translate: TRANSLATE  =
struct

structure F = MipsFrame
structure A = Absyn
structure T = Tree		  

(* Ex: expression - Tree.exp 
   Cx: conditional branch function - fn(true_label, false_label)
   Nx: no result - Tree statement *)		  
datatype exp = Ex of T.exp
	     | Nx of T.stm
	     | Cx of Temp.label * Temp.label -> T.stm
						    
datatype level = Outermost | Level of {frame: F.frame, parent: level} * unit ref	       
type access = level * F.access

val outermost = Outermost
val fragref = ref ([]: F.frag list)		    

fun newLevel ({parent, name, formals}) = Level({frame=F.newFrame({name=name, formals=true::formals}), parent=parent}, ref ())

fun formals (framelevel) =
    case framelevel of
	Outermost => []
      | Level({frame, parent}, unique) =>
	let
	    fun getAccess([]) = []
	      | getAccess(a::l) = (Level({frame=frame, parent=parent}, unique),a)::getAccess(l)
	in
	    getAccess(F.formals(frame))
	end
	    		    
fun allocLocal (framelevel) escape =
    case framelevel of
	Level({frame, parent}, unique) => (framelevel, F.allocLocal(frame) escape)

fun seq ([]) = T.EXP(T.CONST 0)
  | seq ([s]) = s
  | seq (s::l) = T.SEQ(s, seq(l)) 					      

(* Ex constructor *)					      
fun unEx (Ex e) = e
  | unEx (Cx c) =
    let
	val r = Temp.newtemp()
	val t = Temp.newlabel() and f = Temp.newlabel()
    in
	T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
		   c(t,f),
		   T.LABEL f,
		   T.MOVE(T.TEMP r, T.CONST 0),
		   T.LABEL t],
	       T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

(* Cx constructor *)			
fun unCx (Ex (T.CONST 0)) = (fn(t,f) => T.JUMP(T.NAME f, [f]))
  | unCx (Ex (T.CONST 1)) = (fn(t,f) => T.JUMP(T.NAME t, [t]))
  | unCx (Ex e) = (fn(t,f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))				    
  | unCx (Cx c) = c
  | unCx (Nx s) = (fn(t,f) => s) (* raise error, should never occur *) 

(* Nx constructor *)				 
fun unNx (Ex e) = T.EXP(e)
  | unNx (Cx c) = T.EXP(unEx(Cx c))
  | unNx (Nx s) = s

fun intExp (n) = Ex(T.CONST n)

(* TODO: make new label and return Tree.NAME(label), and add to the fragment list *)		   
fun stringExp (s) =
    let
	val lab = Temp.newlabel()
    in
	(fragref := (!fragref) @ [F.STRING(lab, s)];
	 Ex(T.NAME lab))
    end	

fun nilExp () = Ex(T.CONST 0)

fun OpExp (left, A.PlusOp, right) = Ex(T.BINOP(T.PLUS, unEx(left), unEx(right)))
  | OpExp (left, A.MinusOp, right) = Ex(T.BINOP(T.MINUS, unEx(left), unEx(right)))
  | OpExp (left, A.TimesOp, right) = Ex(T.BINOP(T.MUL, unEx(left), unEx(right)))
  | OpExp (left, A.DivideOp, right) = Ex(T.BINOP(T.DIV, unEx(left), unEx(right)))
  | OpExp (left, A.EqOp, right) = Cx(fn(t,f) => T.CJUMP(T.EQ, unEx(left), unEx(right), t, f))
  | OpExp (left, A.NeqOp, right) = Cx(fn(t,f) => T.CJUMP(T.NE, unEx(left), unEx(right), t, f))
  | OpExp (left, A.LtOp, right) = Cx(fn(t,f) => T.CJUMP(T.LT, unEx(left), unEx(right), t, f))
  | OpExp (left, A.LeOp, right) = Cx(fn(t,f) => T.CJUMP(T.LE, unEx(left), unEx(right), t, f))
  | OpExp (left, A.GtOp, right) = Cx(fn(t,f) => T.CJUMP(T.GT, unEx(left), unEx(right), t, f))
  | OpExp (left, A.GeOp, right) = Cx(fn(t,f) => T.CJUMP(T.GE, unEx(left), unEx(right), t, f))

(* translate sequence of expression *)		      
fun seqExp ([]) = Ex(T.CONST 0)
  | seqExp (e::l) = Ex(T.ESEQ(unNx(e), unEx(seqExp(l))))

(* translate assignment *)		      
fun assignExp (var, exp) = Nx(T.MOVE(unEx(var), unEx(exp)))

(* translate let-in-end expression *)
fun letExp ([], body) = (body)
  | letExp (decs, body) = Ex(T.ESEQ(seq(map unNx decs),
				    unEx(body)))

(* A.IfExp{test, then'[, else']} translate if (else) expression *)
fun ifExp (test, then') =
    let
	val condfn = unCx(test)
	val thenExp = unNx(then')			  
	val t = Temp.newlabel() and f = Temp.newlabel()
    in
	Nx(seq[condfn(t,f),
	       T.LABEL t,
	       thenExp,
	       T.LABEL f])
    end

fun ifElseExp (test, then', else') =
    let
	val condfn = unCx(test)
	val thenExp = unEx(then')
	val elseExp = unEx(else')
	val t = Temp.newlabel()
	val f = Temp.newlabel()
	val endlabel = Temp.newlabel()
	val ans = Temp.newtemp()
    in
	Ex(T.ESEQ(seq[condfn(t,f),
		      T.LABEL t,
		      T.MOVE(T.TEMP ans, thenExp),
		      T.JUMP(T.NAME(endlabel), [endlabel]),
		      T.LABEL f,
		      T.MOVE(T.TEMP ans, elseExp),
		      T.LABEL endlabel],
		  T.TEMP ans))
    end

(* translate while loop *)	
fun whileExp (test, body) =
    let
	val condfn = unCx(test)
	val bodyExp = unNx(body)
	val testLabel = Temp.newlabel()
	val bodyLabel = Temp.newlabel()
	val doneLabel = Temp.newlabel()
    in
	Nx(seq[T.LABEL testLabel,
	       condfn (bodyLabel, doneLabel),
	       T.LABEL bodyLabel,
	       bodyExp,
	       T.JUMP(T.NAME testLabel, [testLabel]),
	       T.LABEL doneLabel])
    end
	
(* for loop, avoid overflow *)
fun forExp (var, lo, hi, body) =
    let
	val i = unEx(var)
	val loExp = unEx(lo)
	val hiExp = unEx(hi)
	val bodyExp = unNx(body)
	val bodyLabel = Temp.newlabel()
	val testLabel = Temp.newlabel()
	val increment = Temp.newlabel()
	val endLabel = Temp.newlabel()
    in
	Nx(seq[T.MOVE(i, loExp),
	       T.CJUMP(T.LE, i, hiExp, bodyLabel, endLabel),
	       T.LABEL increment,
	       T.MOVE(i, T.BINOP(T.PLUS, i, T.CONST 1)),
	       T.LABEL bodyLabel,
	       bodyExp,
	       T.CJUMP(T.LT, i, hiExp, increment, endLabel),
	       T.LABEL endLabel])
    end

(* break expression, simply jump to done label passed in*)	
fun breakExp (done) = Nx(T.JUMP(T.NAME(done), [done]))

(* translate function call *)			
fun callExp (level, label, formals) = Ex(T.CALL(T.NAME(label), map unEx formals))

(* translate record creation *)					
fun recordExp ({length, fields}) =
    let
	val r = T.TEMP(Temp.newtemp())
	fun initRecord ([], offset, result, r) = result
	  | initRecord (a::l, offset, result, r) =
	    initRecord(l, offset+1, [(T.MOVE(T.MEM(T.BINOP(T.PLUS, r, T.BINOP(T.MUL, T.CONST(offset), T.CONST(F.wordsize)))), unEx(a)))] @ result, r)
    in
	Ex(T.ESEQ(seq[T.MOVE(r, F.externalCall("malloc", [T.BINOP(T.MUL, T.CONST(length), T.CONST(F.wordsize))])),
		      seq(initRecord(fields, 0, [], r))],
		  r))
    end	

(* translate array creation *)
fun arrayExp (length, value) =
    let
	val r = T.TEMP(Temp.newtemp())
	val a = unEx(length)
	val b = unEx(value)
    in
	Ex(T.ESEQ(T.MOVE(r, F.externalCall("initArray", [a, b])),
		  r))
    end

(* TODO *)
fun simpleVar ((level_dec, access_dec): access, level_use: level) =
    let
	val Level(frame_dec, defaultRef) = level_dec
	fun followStaticLinks (Level({parent, frame}, currentRef): level, currentAccess: T.exp) =
	    if defaultRef = currentRef then
		F.exp(access_dec) (currentAccess)
	    else
		followStaticLinks(parent, F.exp(List.hd(F.formals(frame))) (currentAccess))
	  | followStaticLinks (_,_) = ErrorMsg.impossible "no static link to follow" 
    in
	Ex(followStaticLinks(level_use, T.TEMP(F.FP)))
    end	
						     
fun arrayVar (var, index) =
    let
	val varExp = unEx(var)
	val indexExp = unEx(index)
	val offset = T.BINOP(T.MUL, indexExp, T.CONST(F.wordsize))
    in
	Ex(T.MEM(T.BINOP(T.PLUS, varExp, offset)))
    end

fun procEntryExit ({level, body}) =
    let
	val Level({frame, parent}, unique) = level
	val body' = F.procEntryExit1(frame, unNx(body))				    
	val move = T.MOVE((T.TEMP F.RV), unEx(Nx(body')))
	val frag = F.PROC({body=move, frame=frame})
    in
	(fragref := frag::(!fragref);())
    end

fun getResult () = !fragref	
						    
end
