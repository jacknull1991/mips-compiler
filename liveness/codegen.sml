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

	val calldefs = F.callersaves @ [F.RA, F.RV]

	fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
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
	  | munchStm(T.MOVE(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)),e2))=
		emit(A.OPER{assem="sw `s0, " ^ int(~i) ^ "(`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
	    emit(A.OPER{assem="sw `s0, " ^ int i ^ "(r0)\n",
			src=[munchExp e2],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    emit(A.OPER{assem="sw `s0, (`s1)\n",
			src=[munchExp e2, munchExp e1],
			dst=[],
			jump=NONE})
	  | munchStm (T.MOVE(T.TEMP i, T.CONST j)) =
	    emit(A.MOVE{assem="move `d0, `s0\n",
			src=j,
			dst=i})
	  | munchStm (T.MOVE(e1, e2)) =
	    emit(A.MOVE{assem="move `d0, `s0\n",
			src=munchExp e2,
			dst=munchExp e1})
		
	  | munchStm (T.JUMP(T.TEMP ra, _)) =
	    emit(A.OPER{assem="jr `s0\n",
			src=[ra],
			dst=[],
			jump=NONE})
	  | munchStm (T.JUMP(T.NAME label, l)) =
	    emit(A.OPER{assem="j `j0\n",
			src=[],
			dst=[],
			jump=SOME(l)})
	  | munchStm (T.JUMP(e1, l)) =
	    emit(A.OPER{assem="jr `j0\n",
			src=[munchExp e1],
			dst=[],
			jump=SOME(l)})
	  | munchStm (T.CJUMP(relop, e1, e2, tl, fl)) =
	    emit(A.OPER{assem=relopname(relop) ^ " `s0, `s1, " ^ S.name(tl) ^ "\nj " ^ S.name(fl) ^ "\n",
			src=[munchExp e1, munchExp e2],
			dst=[],
			jump=SOME([tl, fl])})
		
	  | munchStm (T.LABEL lab) =
	    emit(A.LABEL{assem=S.name(lab) ^ ":\n",
			 lab=lab})
		
	  | munchStm (T.EXP(e)) = (munchExp e; ())

	and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
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
	  | munchExp (T.MEM(T.CONST i)) =
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, " ^ int i ^ "(r0)\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.MEM(e1)) =
	    (result(fn r => emit(A.OPER
				    {assem="lw `d0, 0(`s0)\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))

	  | munchExp (T.BINOP(binop, e1, T.CONST i)) =
	    (result(fn r => emit(A.OPER
				    {assem=binopname(binop) ^ "i `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(binop, T.CONST i, e1)) =
	    (result(fn r => emit(A.OPER
				    {assem=binopname(binop) ^ "i `d0, `s0, " ^ int i ^ "\n",
				     src=[munchExp e1],
				     dst=[r],
				     jump=NONE})))
	  | munchExp (T.BINOP(binop, e1, e2)) =
	    (result(fn r => emit(A.OPER
				    {assem=binopname(binop) ^ " `d0, `s0, `s1\n",
				     src=[munchExp e1, munchExp e2],
				     dst=[r],
				     jump=NONE})))

	  | munchExp (T.TEMP t) = (t)

	  | munchExp (T.ESEQ _) = ErrorMsg.impossible "MunchExp Error: ESEQ stm not eliminated!"

	  | munchExp (T.NAME label) =
	    (result(fn r => emit(A.OPER
				    {assem="la `d0, " ^ S.name(label) ^ "\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))

	  | munchExp (T.CONST i) =
	    (result(fn r => emit(A.OPER
				    {assem="li `d0, " ^ int i ^ "\n",
				     src=[],
				     dst=[r],
				     jump=NONE})))

	  | munchExp (T.CALL(T.NAME(f), args)) =
	    (emit(A.OPER{assem="jal " ^ S.name(f) ^ "\n",
			 src=munchExp(T.NAME(f))::munchArgs(0, args),
			 dst=calldefs,
			 jump=NONE});
	     F.RV)

	and munchArgs (i, []) = []
	  | munchArgs (i, a::l) =
	    let
	    in
		if i < 4 then
		    (munchStm(T.MOVE(T.TEMP (List.nth(F.argregs, i)), T.TEMP (munchExp(a))));
		     List.nth(F.argregs, i)::munchArgs(i+1, l))
		else
		    ErrorMsg.impossible "Arguments Error: only 4 argument registers available!"
	    end
    in
	munchStm stm;
	rev(!ilist)
    end	
						    
end
