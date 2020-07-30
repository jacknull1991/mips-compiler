signature CODEGEN =
sig
    val codegen: MipsFrame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen: CODEGEN =
struct

structure A = Assem
structure T = Tree
structure S = Symbol
structure F = MipsFrame

fun codegen (frame) (stm) : A.instr list =
    let
	val ilist = ref (nil: A.instr list)
	fun emit x = ilist := x::(!ilist)
	fun result (gen) =
	    let
		val t = Temp.newtemp()
	    in
		gen t;
		t
	    end

	fun int (i) =
	    if i >= 0 then
		Int.toString(i)
	    else
		"-" ^ Int.toString(~i)

	fun relopname (relop: T.relop) =
	    case relop of
		T.EQ => "beq"
	      | T.NE => "bne"
	      | T.LT => "blt"
	      | T.LE => "ble"
	      | T.GT => "bgt"
	      | T.GE => "bge"
	      | _ => "undefined"

	fun binopname (binop: T.binop) =
	    case binop of
		T.PLUS => "add"
	      | T.MINUS => "sub"
	      | T.MUL => "mul"
	      | T.DIV => "div"
	      | T.AND => "and"
	      | T.OR => "or"
	      | T.LSHIFT => "sll"
	      | T.RSHIFT => "srl"
	      | T.ARSHIFT => "sra"
	      | T.XOR => "xor"

	fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
	  (* store back to memory *)
	  (* MEM(e1+/-i) <- e2 *)					 
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
	    emit(A.OPER{assem="sw `s0, " ^ int i ^ "(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
	    emit(A.OPER{assem="sw `s0, " ^ int i ^ "(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)), e2)) =
	    emit(A.OPER{assem="sw `s0, " ^ int(~i) ^ "(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.MINUS, T.CONST i, e1)), e2)) =
	    emit(A.OPER{assem="sw `s0, " ^ int(~i) ^ "(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  (* MEM(i) <- e *)		
	  | munchStm (T.MOVE(T.MEM(T.CONST i), e)) =
	    emit(A.OPER{assem="sw `s0, " ^ int i ^ "(`s1)\n",
			src=[munchExp e],
			dst=[],
			jump=NONE})
	  (* MEM(e1) <- e2 *)		
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    emit(A.OPER{assem="sw `s0, 0(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  (* load into register *)
	  (* R(t) <- i *)		
	  | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
	    emit(A.OPER{assem="li `d0, " ^ int i ^ "\n",
			src=[],
			dst=[t],
			jump=NONE})
	  (* R(t) <- MEM(e+/-i) *)
	  | munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, T.CONST i, e)))) =
	    emit(A.OPER{assem="lw `d0, " ^ int i ^ "(`s0)\n",
			src=[munchExp e],
			dst=[t],
			jump=NONE})
	  | munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))) =
	    emit(A.OPER{assem="lw `d0, " ^ int i ^ "(`s0)\n",
			src=[munchExp e],
			dst=[t],
			jump=NONE})
	  | munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.MINUS, T.CONST i, e)))) =
	    emit(A.OPER{assem="lw `d0, " ^ int(~i) ^ "(`s0)\n",
			src=[munchExp e],
			dst=[t],
			jump=NONE})
	  | munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.MINUS, e, T.CONST i)))) =
	    emit(A.OPER{assem="lw `d0, " ^ int(~i) ^ "(`s0)\n",
			src=[munchExp e],
			dst=[t],
			jump=NONE})
	  (* R(t) <- addr *)		
	  | munchStm (T.MOVE(T.TEMP t, T.NAME lable)) =
	    emit(A.OPER{assem="la `d0, " ^ S.name(lable) ^ "\n",
			src=[],
			dst=[t],
			jump=NONE})
	  (* register to register *)		
	  | munchStm (T.MOVE(T.TEMP t, e)) =
	    emit(A.MOVE{assem="move `d0, `s0\n",
			src=munchExp e,
			dst=t})
	  (* jumps *)
	  (* jump to target label *)		
	  | munchStm (T.JUMP(T.NAME label, l)) =
	    emit(A.OPER{assem="j " ^ S.name(label) ^ "\n",
			src=[],
			dst=[],
			jump=SOME(l)})
	  (* jump to addr in register *)		
	  | munchStm (T.JUMP(e, l)) =
	    emit(A.OPER{assem="jr `s0\n",
			src=[munchExp e],
			dst=[],
			jump=SOME(l)})
	  (* conditional jump, always fall through to false label *)		
	  | munchStm (T.CJUMP(relop, e1, e2, tl, fl)) =
	    emit(A.OPER{assem=relopname(relop) ^ " `s0, `s1, " ^ S.name(tl) ^ "\n",
			src=[munchExp e1, munchExp e2],
			dst=[],
			jump=SOME([tl])})
	  (* label *)		
	  | munchStm (T.LABEL lab) =
	    emit(A.LABEL{assem=S.name(lab) ^ ":\n",
			 lab=lab})
	  (* call to munchExp *)		
	  | munchStm (T.EXP(e)) = (munchExp e; ())

	  | munchStm (e) =
	    (Printtree.printtree(TextIO.stdOut, e);
	     ErrorMsg.impossible "Exception: couldn't assign instruction to statement")

	and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
	    (* load from memory *)
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, " ^ int i ^ "(`s0)\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, " ^ int i ^ "(`s0)\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.MEM(T.BINOP(T.MINUS, e1, T.CONST i))) =
	    (result(fn r => emit(A.OPER
				     {assem="lw `d0, " ^ int(~i) ^ "(`s0)\n",
				      src=[munchExp e1],
				      dst=[r],
				      jump=NONE})))
	  | munchExp (T.MEM(T.BINOP(T.MINUS, T.CONST i, e1))) =
	    (result(fn r => emit(A.OPER
				     {assem="lw `d0, " ^ int(~i) ^ "(`s0)\n",
				      src=[munchExp e1],
				      dst=[r],
				      jump=NONE})))		
	  | munchExp (T.MEM(T.CONST i)) =
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, " ^ int i ^ "(`s0)\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.MEM(e1)) =
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, 0(`s0)\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  (* binary operations *)
	  (* zeros *)		
	  | munchExp (T.BINOP(T.PLUS, T.CONST 0, T.CONST i)) =
	    (result(fn r => emit(A.OPER
				     {assem="addi `d0, `s0, " ^ int i ^ "\n",
				      src=[F.ZERO],
				      dst=[r],
				      jump=NONE})))
	  | munchExp (T.BINOP(T.PLUS, T.CONST i, T.CONST 0)) =
	    (result(fn r => emit(A.OPER
				     {assem="addi `d0, `s0, " ^ int i ^ "\n",
				      src=[F.ZERO],
				      dst=[r],
				      jump=NONE})))
	  | munchExp (T.BINOP(T.MINUS, T.CONST 0, T.CONST i)) =
	    (result(fn r => emit(A.OPER
				     {assem="addi `d0, `s0, " ^ int(~i) ^ "\n",
				      src=[F.ZERO],
				      dst=[r],
				      jump=NONE})))
	  | munchExp (T.BINOP(T.MINUS, T.CONST i, T.CONST 0)) =
	    (result(fn r => emit(A.OPER
				     {assem="addi `d0, `s0, " ^ int(~i) ^ "\n",
				      src=[F.ZERO],
				      dst=[r],
				      jump=NONE})))
	  (* immediate *)		
	  | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) =
	    (result(fn r => emit(A.OPER
				    {assem="addi `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) =
	    (result(fn r => emit(A.OPER
				    {assem="addi `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(T.AND, e1, T.CONST i))  =
	    (result(fn r => emit(A.OPER
				    {assem="andi `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(T.AND, T.CONST i, e1))  =
	    (result(fn r => emit(A.OPER
				    {assem="andi `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(T.OR, e1, T.CONST i))  =
	    (result(fn r => emit(A.OPER
				    {assem="ori `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
          | munchExp (T.BINOP(T.OR, T.CONST i, e1))  =
	    (result(fn r => emit(A.OPER
				    {assem="ori `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
          | munchExp (T.BINOP(T.XOR, e1, T.CONST i))  =
	    (result(fn r => emit(A.OPER
				    {assem="xori `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
          | munchExp (T.BINOP(T.XOR, T.CONST i, e1))  =
	    (result(fn r => emit(A.OPER
				    {assem="xori `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(binop, e1, e2)) =
	    (result(fn r => emit(A.OPER
				    {assem=binopname(binop) ^ " `d0, `s0, `s1\n",
				     src=[munchExp e1, munchExp e2],
				     dst=[r],
				     jump=NONE})))
	  (* register *)		
	  | munchExp (T.TEMP t) = (t)

	  | munchExp (T.ESEQ _) = ErrorMsg.impossible "MunchExp Error: ESEQ stm not eliminated!"
	  (* load label addr *)						      
	  | munchExp (T.NAME label) =
	    (result(fn r => emit(A.OPER
				    {assem="la `d0, " ^ S.name(label) ^ "\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))
	  (* load immediate *)		
	  | munchExp (T.CONST i) =
	    (result(fn r => emit(A.OPER
				    {assem="li `d0, " ^ int i ^ "\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))
	  (* function call *)		
	  | munchExp (T.CALL(T.NAME(f), args)) =
	    (emit(A.OPER{assem="jal " ^ S.name(f) ^ "\n",
			 src=munchArgs(0, args),
			 dst=F.RA::F.RV::F.calleesaves,
			 jump=NONE});
	     F.RV)
		
	  | munchExp (e) =
	    (Printtree.printtree(TextIO.stdOut, T.EXP(e));
	     ErrorMsg.impossible "Compiling error: couldn't assign instruction to expression")

	and munchArgs (i, []) = []
	  | munchArgs (i, a::l) =
	    let
	    in
		if i < 4 then
		    (munchStm(T.MOVE(T.TEMP (List.nth(F.argregs, i)), T.TEMP (munchExp(a))));
		     List.nth(F.argregs, i)::munchArgs(i+1, l))
		else
		    (* not enough registers, save arg on frame *)
		    (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(F.SP), T.CONST(F.wordsize * (i-4)))), a));
		     munchArgs(i+1, l))
	    end
    in
	munchStm stm;
	rev(!ilist)
    end	
						    
end
