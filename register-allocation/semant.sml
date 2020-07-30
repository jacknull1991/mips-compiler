signature SEMANT =
sig
    type venv
    type tenv
    type expty

    val transProg: Absyn.exp -> {frags: MipsFrame.frag list, ty: Types.ty}
end				    

structure Semant: SEMANT
=
struct
(* abbreviations *)	       
structure A = Absyn
structure S = Symbol
structure T = Types
structure E = Env
structure Tr = Translate		   
		  
type venv = Env.enventry S.table
type tenv = T.ty S.table
type expty = {exp: Tr.exp, ty: T.ty} 
val error = ErrorMsg.error

val breakable = ref false
val looplevel = ref 0		    

structure binarySet = BinarySetFn
(
  struct
  type ord_key = string
  val compare = String.compare
  end
)

fun actual_ty (ty:T.ty, pos) =
    case ty of
	T.NAME (symb, tyref) =>
	(case !tyref of
             NONE => (error pos ("Undefined actual type: " ^ S.name(symb)); T.BOTTOM)
	   | SOME t => actual_ty (t, pos))
      | T.ARRAY(t, unique) => T.ARRAY(actual_ty(t, pos), unique)
      | _ => ty

fun checkInt ({exp = _, ty = t}, pos) =
    case actual_ty(t, pos) of
	T.INT => ()
      | T.BOTTOM => ()			
      | _ => error pos ("Expected: INT, Actual: " ^ T.name(t))

fun checkUnit ({exp = _, ty = t}, pos) =
    case actual_ty(t,pos) of
	T.UNIT => ()
      | T.BOTTOM => ()
      | _ => error pos ("Expected: UNIT, Actual: " ^ T.name(t))
			  
fun checkString ({exp = _, ty = t}, pos) =
    case actual_ty(t,pos) of
	T.STRING => ()
      | T.BOTTOM => ()
      | _ => error pos ("Expected: STRING, Actual: " ^ T.name(t))

fun checkRecord ({exp = _, ty = t}, pos) =
    case actual_ty(t, pos) of
	T.RECORD _ => ()
      | T.BOTTOM => ()		
      | T.NIL => ()
      | _ => error pos ("Expected: RECORD, Actual: " ^ T.name(t))

fun checkArray ({exp = _, ty = t}, pos) =
    case actual_ty(t,pos) of
	T.ARRAY _=> ()
      | T.BOTTOM => ()
      | _ => error pos ("Expected: ARRAY, Actual: " ^ T.name(t))			  
	       
fun lookupType (tenv, ty, pos) =
    case S.look(tenv, ty) of
	NONE => (error pos ("Type undefined: " ^ S.name(ty)); T.BOTTOM)
      | SOME ty => actual_ty(ty, pos)

fun hasSameType (ty1: T.ty, ty2: T.ty, pos1: A.pos, pos2: A.pos) =
    let
	val t1 = actual_ty(ty1, pos1)
	val t2 = actual_ty(ty2, pos2)
    in
	if t1 = t2 then true
	else if t1 = T.NIL then
	    (case t2 of
		 T.RECORD(fields, unique) => true
	       | T.BOTTOM => true				 
	       | _ => false)
	else if t2 = T.NIL then
	    (case t1 of
		 T.RECORD(fields, unique) => true
	       | T.BOTTOM => true				 
	       | _ => false)
	else
	    false
    end

fun transExp (venv, tenv, level, donelabel) =
    let
	datatype operClass = ArithOp | CmpOp | EqNeqOp
	fun classOp oper =
	    case oper of
		A.PlusOp => ArithOp
	      | A.MinusOp => ArithOp
	      | A.TimesOp => ArithOp
	      | A.DivideOp => ArithOp
	      | A.LtOp => CmpOp
	      | A.LeOp => CmpOp
	      | A.GtOp => CmpOp
	      | A.GeOp => CmpOp
	      | A.EqOp => EqNeqOp
	      | A.NeqOp => EqNeqOp
	fun trexp (A.VarExp var) = trvar var
	  | trexp (A.NilExp) = {exp = Tr.nilExp(), ty = T.NIL} 
	  | trexp (A.IntExp n) = {exp = Tr.intExp(n), ty = T.INT}
	  | trexp (A.StringExp (s,pos)) = {exp = Tr.stringExp(s), ty = T.STRING}
	  | trexp (A.CallExp{func, args, pos}) =
	    (case S.look(venv, func) of
		NONE => (error pos ("Function undefined: " ^ S.name(func));
			 {exp = Tr.nilExp(), ty = T.BOTTOM})
	      | SOME (E.VarEntry _) =>
		(error pos "Function expected, but variable found";
		 {exp = Tr.nilExp(), ty = T.BOTTOM})
	      | SOME (E.FunEntry {level=level_dec, label, formals, result}) =>
		if (List.length args <> List.length formals) then
		    (error pos "Number of arguments does not match parameters";
		     {exp = Tr.nilExp(), ty = T.BOTTOM})
		else
		    let
			val arg_forms = ListPair.zipEq(args, formals)
			val arg_exps = map trexp args					   
			fun check_exp_ty (arg, form) =
			    let
				val argty = #ty (trexp arg)
			    in
				if
				    form <> argty
				then
				    (error pos ("Type mismatch between argument and parameter\nExpected: " ^ T.name(form) ^ ", Actual: " ^ T.name(argty));
				     ())
				else
				    ()
			    end				
		    in
			(List.app check_exp_ty arg_forms;
			 {exp = Tr.callExp(level, level_dec, label, map (#exp) arg_exps), ty = actual_ty(result, pos)})
		    end
	    )
	  | trexp (A.OpExp{left, oper, right, pos}) =
	    let
		val {exp=leftExp, ty=leftType} = trexp left
		val {exp=rightExp, ty=rightType} = trexp right
	    in
		(case classOp oper of
		     ArithOp =>
		     (checkInt({exp=leftExp, ty=leftType}, pos);
		      checkInt({exp=rightExp, ty=rightType}, pos);      
		      {exp = Tr.OpExp(leftExp, oper, rightExp), ty = T.INT})			 
 		   | CmpOp =>		     
		     (case leftType of			  
			  T.INT => (checkInt({exp=rightExp, ty=rightType}, pos);
			  	    {exp = Tr.OpExp(leftExp, oper, rightExp), ty = T.INT})			      
			| T.STRING => (checkString({exp=rightExp, ty=rightType}, pos);			   
				       {exp = Tr.StringOpExp(leftExp, oper, rightExp), ty = T.STRING})
			| _ => (error pos "Type mismatch between operands of comparison expression";
				{exp = Tr.nilExp(), ty = T.BOTTOM}))
		   | EqNeqOp =>
		     (case leftType of
			  T.INT => (checkInt({exp=rightExp, ty=rightType}, pos);
				    {exp = Tr.OpExp(leftExp, oper, rightExp), ty = T.INT})
			| T.STRING => (checkString({exp=rightExp, ty=rightType}, pos);
				       {exp = Tr.StringOpExp(leftExp, oper, rightExp), ty = T.INT})
			| _ => (error pos "Type mismatch between operands of comparison expression";
				{exp = Tr.nilExp(), ty = T.BOTTOM})))
	    end		
	  | trexp (A.RecordExp {fields, typ, pos}) =
	    (case S.look(tenv, typ) of
		NONE =>  (error pos ("Undefined record type: " ^ S.name(typ));
			  {exp = Tr.nilExp(), ty = T.BOTTOM})
	      | SOME t' =>		
		let
		    val t  = actual_ty (t', pos)
		in
		    (case t of
			 T.RECORD (rec_lst, unique) =>
			 let
			     fun checkField ((dec_symbol, dec_ty), (init_symbol, init_exp, pos)) =
				 let
				     val {exp=initExp, ty=init_ty} = trexp init_exp
				 in
				     (* record field must be in order *)
				     if (S.name dec_symbol) = (S.name init_symbol) then
					 if hasSameType(dec_ty, init_ty, pos, pos) then
					     ()
					 else
					     (error pos ("Type mismatch between field type and expression");())
				     else
					 (error pos ("Unmatched record field.\nExpected: " ^ S.name(dec_symbol) ^ "\nActual: " ^ S.name(init_symbol));())
				 end				     				
			     fun checkFields rec_lst =
				 if List.length rec_lst <> List.length fields then
				     (error pos "Number of fields in record does not match definition"; ())
				 else
				     List.app checkField (ListPair.zipEq(rec_lst, fields))
			     fun translateField ([], result) = result
			       | translateField ((symbol, exp, pos)::l, result) =
				 let
				     val field' = trexp exp
				 in
				     translateField(l, result @ [(#exp field')])
				 end				     
			 in
			     (checkFields rec_lst;
			      {exp = Tr.recordExp({length=List.length(fields), fields=translateField(fields, [])}), ty = t})
			 end
		       | _ => (error pos ("Undefined record type " ^ T.name(actual_ty(t',pos)));
			       {exp = Tr.nilExp(), ty = T.BOTTOM}))
		end
	    )
	  | trexp (A.SeqExp exps) =
	    let
		fun trexps ([], translatedExps, currentType) = {exp = Tr.seqExp(translatedExps), ty = currentType}
		  | trexps ((exp, pos)::l, translatedExps, currentType) =
		    let
			val {exp=exp', ty=ty'} = trexp exp
		    in
			trexps(l, translatedExps @ [exp'], ty')
		    end
	    in
		trexps(exps, [], T.UNIT)
	    end
	  | trexp (A.AssignExp {var, exp, pos}) =
	    let
		val {exp=var', ty=varType} = trvar var
		val {exp=exp', ty=expType} = trexp exp
	    in
		if hasSameType(varType, expType, pos, pos) then
		    {exp = Tr.assignExp(var', exp'), ty = T.UNIT}
		else
		    (error pos ("Type mismatch between LHS and RHS of assignment \nLHS: " ^ T.name(actual_ty(varType,pos)) ^ "\nRHS: " ^ T.name(actual_ty(expType,pos)));
		     {exp = Tr.nilExp(), ty = T.BOTTOM})
	    end
	  | trexp (A.IfExp {test, then', else' = NONE, pos}) =
	    let
		val test' = trexp test
		val then'' = trexp then'
	    in
		(checkInt(test', pos);
		 checkUnit(then'', pos);
		 {exp = Tr.ifExp(#exp test', #exp then''), ty = T.UNIT})
	    end		
	  | trexp (A.IfExp {test, then', else' = SOME else'', pos}) =
	    let
		val test' = trexp test
		val {exp=thenExp, ty=thenType} = trexp then'
		val {exp=elseExp, ty=elseType} = trexp else''						       
	    in
		(checkInt(test', pos);
		 if hasSameType(thenType, elseType, pos, pos) then
		     {exp = Tr.ifElseExp(#exp test', thenExp, elseExp), ty = thenType}
		 else
		     (error pos "Type mismatch between THEN and ELSE expressions";
		      {exp = Tr.nilExp(), ty = T.BOTTOM}))
	    end
	  | trexp (A.WhileExp {test, body, pos}) =
	    let
		val test' = trexp test
	    in
		looplevel := !looplevel + 1;
		breakable := true;
		let
		    val body' = trexp body
		in		    
		    (checkInt (test',pos);
		     checkUnit (body', pos);
		     looplevel := !looplevel - 1;
		     {exp = Tr.whileExp(#exp test', #exp body'), ty = T.UNIT})
		end
	    end		
	  | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
	    let
		val accesslevel = Tr.allocLocal level (!escape)
		val venv' = S.enter(venv, var, E.VarEntry {access=accesslevel, ty = T.INT})
		val lo' = transExp(venv', tenv, level, donelabel) lo
		val hi' = transExp(venv', tenv, level, donelabel) hi
	    in
		looplevel := !looplevel + 1;
		breakable := true;
		let
		    val body' = transExp(venv', tenv, level, donelabel) body
		in
		    (checkInt (lo', pos);
		     checkInt (hi', pos);
		     checkUnit (body', pos);
		     looplevel := !looplevel - 1;
		     {exp = Tr.forExp(Tr.simpleVar(accesslevel, level), #exp lo', #exp hi', #exp body'), ty = T.UNIT})
		end 
	    end
	  | trexp (A.BreakExp pos) =
	    if (!looplevel = 0) then
		(error pos "Break statement outside of loop"; {exp = Tr.nilExp(), ty = T.BOTTOM})
	    else if !breakable = false then
		(error pos "Repeated break statement"; {exp = Tr.nilExp(), ty = T.BOTTOM})
	    else
		(breakable := false; {exp = Tr.breakExp(donelabel), ty = T.UNIT})
	  | trexp (A.LetExp{decs, body, pos}) =
	    let
		fun trdecs (venv, tenv, decs, explist) =
		    (case decs of
			 [] => {venv=venv, tenv=tenv, explist=explist}
		       | (dec::l) =>
			 let
			     val {venv=venv', tenv=tenv', explist=explist'} = transDec(venv, tenv, level, donelabel, explist) dec
			 in
			     trdecs(venv', tenv', l, explist')
			 end
		    )
		val {venv=venv', tenv=tenv', explist=explist'} = trdecs(venv, tenv, decs, [])
		val {exp=decExp, ty=decType} = transExp(venv', tenv', level, donelabel) body
	    in
		{exp=Tr.letExp(explist', decExp), ty=decType}
	    end
	  | trexp (A.ArrayExp {typ, size, init, pos}) =
	    (case S.look(tenv, typ) of
		 NONE =>  (error pos ("Undefined array type:" ^ S.name(typ));
			   {exp = Tr.nilExp(), ty = T.BOTTOM})
	       | SOME t =>
		 let
		     val size' = trexp size
		     val init' = trexp init
		 in
		     (case actual_ty(t, pos) of
			  T.ARRAY (ty, unique) =>
			  (checkInt(size', pos);
			   if hasSameType(ty, #ty init', pos, pos) then
			       {exp = Tr.arrayExp(#exp size', #exp init'), ty = t}
			   else
			       (error pos "Type mismatch between array type and expression";
				{exp = Tr.nilExp(), ty = T.BOTTOM}))
			| _ => (error pos "Undefined array type";
				{exp = Tr.nilExp(), ty = T.BOTTOM}))
		 end
	    )	
	and trvar (A.SimpleVar(id, pos)) =
	    (case S.look(venv, id) of
		 SOME(E.VarEntry {access, ty}) => {exp = Tr.simpleVar(access, level), ty = actual_ty(ty,pos)}
	       | _ => (error pos ("Undefined variable " ^ S.name id);
		       {exp = Tr.nilExp(), ty = T.BOTTOM}))
	  | trvar (A.FieldVar (var, id, pos)) =
	    let
		val {exp=varExp, ty=varType} = trvar var
		fun indexOf (item, items) =
		    let
			fun index (i, []) = i
			  | index (i, a::l) = if item = a then i else index(i+1, l)
		    in
			index (0, items)
		    end			
	    in
		case varType of
		    T.RECORD (fields, unique) =>
		    (case List.find (fn (s, t) => s = id) fields of
			 NONE => (error pos ("Undefined record field: " ^ S.name id);
				  {exp = Tr.nilExp(), ty = T.BOTTOM})
		       | SOME (s, t) =>
			 let
			     val fieldIndex = indexOf((s,t), fields)
			 in
			     {exp = Tr.fieldVar(varExp, fieldIndex), ty = actual_ty(t, pos)}
			 end
		    )
		  | _ => (error pos "Variable should be of type record";
			  {exp = Tr.nilExp(), ty = T.BOTTOM})
	    end
	  | trvar (A.SubscriptVar (var, exp, pos)) =
	    let
		val {exp=varExp, ty=varType} = trvar var
	    in
		case varType of
		    T.ARRAY (tyArr, unique) =>
		    let
			val {exp=expExp, ty=expType} = trexp exp
		    in
			case expType of
			    T.INT => {exp = Tr.subscriptVar(varExp, expExp), ty = actual_ty(tyArr, pos)}
			  | _ => (error pos ("Array subscript should be of type INT. Actual: " ^ T.name(expType));
				  {exp = Tr.nilExp(), ty = T.BOTTOM})
		    end
		  | _ => (error pos ("Variable of array type expected. Actual: " ^ T.name(varType));
			  {exp = Tr.nilExp(), ty = T.BOTTOM})
	    end
    in
	trexp
    end
and transDec (venv, tenv, level, donelabel, explist) =
    let
	fun trdec (A.VarDec {name, escape, typ, init, pos}) =
	    let
		val {exp=initExp, ty=initType} = transExp(venv, tenv, level, donelabel) init
		val accesslevel = Tr.allocLocal level (!escape)
		val explist' = Tr.assignExp(Tr.simpleVar(accesslevel, level), initExp)::explist
	    in
		(case initType of
		     T.NIL =>
		     (case typ of
			  NONE => (error pos "Variable type unknown";
				   {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=T.NIL}), tenv=tenv, explist=explist'})
			| SOME (sym,p) =>
			  (case S.look(tenv, sym) of
			       SOME t =>
			       (case actual_ty(t, p) of
				    T.RECORD(fields, unique) =>
				    {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=actual_ty(t, p)}), tenv=tenv, explist=explist'}
				  | _ => (error pos ("Variable should be of type record. Actual: " ^ S.name(sym));
					  {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=T.NIL}), tenv=tenv, explist=explist'})
			       )
			     | NONE => (error pos ("Undefined type: " ^ S.name(sym));
					{venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=T.NIL}), tenv=tenv, explist=explist'})
			  )
		     )
		   | _ =>
		     (case typ of
			  NONE => {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=initType}), tenv=tenv, explist=explist'}
			| SOME (sym, p) =>
			  (case S.look(tenv, sym) of
			       SOME t =>
			       if hasSameType(t, initType, p, p) then
				   {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=initType}), tenv=tenv,  explist=explist'}
			       else
				   (error pos ("Type mismatch in variable type and initialization expression: " ^ T.name(t) ^ " * " ^ T.name(initType));
				    {venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=initType}), tenv=tenv, explist=explist'})
			     | NONE => (error pos ("Undefined type: " ^ S.name(sym));
					{venv=S.enter(venv, name, E.VarEntry{access=accesslevel, ty=initType}), tenv=tenv, explist=explist'})
			  )
		     )
		)
	    end
	  | trdec (A.TypeDec decs) =
	    let
		val tenv' = List.foldr (fn ({name, ty, pos}, env) =>
					   S.enter(env, name, T.NAME(name, ref NONE)))
				       tenv decs
		val tenv'' = List.foldr (fn ({name, ty, pos}, env) =>
					    (case S.look(env, name) of
						 SOME(T.NAME(n,r)) =>
						 (r := SOME(transTy(env, ty)); env)
					       | _ => (error pos ("Mutually recursive type undefined: " ^ S.name(name)); env))) tenv' decs
		(* check if there is cycle starting with type name *)
		fun findCycle (name:A.symbol, ty:T.ty, pos: A.pos, repeated) =
		    case ty of
			T.NAME(sym, tyref) =>
			if (name=sym andalso repeated=true) then
			    (error pos ("Illegal cycle in type declaration: " ^ S.name(sym)); true)
			else
			    (case !tyref of
				 SOME(t) => findCycle(name, t, pos, true)
			       | NONE => (error pos ("Type undefined: " ^ S.name(sym)); false))
		      | _ => false
		(* check if there is cycle in declaration list *)
		fun checkCycle ([]) = false
		  | checkCycle ({name, ty, pos}::types) =
		    case S.look(tenv'', name) of
			SOME(t) => if findCycle(name, t, pos, false) then true else checkCycle(types)
		      | NONE => (error pos ("Type undefined: " ^ S.name(name)); true)
		(* check repeated type names *)
		fun checkRepeatedName (decs) =
		    let
			fun addName({name, ty, pos}, set) =
			    if binarySet.member(set, S.name(name)) then
				(error pos ("Repeated type name: " ^ S.name(name)); set)
			    else
				binarySet.add(set, S.name(name))
			val allnames = List.foldr addName binarySet.empty decs
		    in
			if binarySet.numItems(allnames) = List.length(decs) then
			    false
			else
			    true
		    end
	    in
		if (checkCycle(decs) orelse checkRepeatedName(decs)) then
		    {venv=venv, tenv=tenv, explist=explist}
		else
		    {venv=venv, tenv=tenv'', explist=explist}
	    end
	  | trdec (A.FunctionDec decs) =
	    let
		fun resultTy res =
		    case res of
			NONE => T.UNIT
		      | SOME(rt, pos) =>
			(case S.look(tenv, rt) of
			     NONE => (error pos ("Return type undefined: " ^ S.name(rt)); T.UNIT)
			   | SOME(t) => t)
		(* add params and return type *)
		fun processHeader ({name, params, result, body, pos}, venv) =
		    let
			val res_ty =resultTy result
			fun transparam ({name, escape, typ, pos}) =
			    case S.look(tenv, typ) of
				SOME(t) => {name=name, ty=t}
			      | NONE => (error pos ("Parameter's type undefined: " ^ S.name(typ));
					 {name=name, ty=T.BOTTOM})
			val params' = map transparam params
			val newlabel = Temp.newlabel()
			val newlevel = Tr.newLevel({parent=level, name=newlabel, formals=map (fn {name, escape, typ, pos} => !escape) params})
		    in
			S.enter(venv, name, E.FunEntry{level=newlevel, label=newlabel, formals=map #ty params', result=res_ty})
		    end
		(* handle function body *)
		fun processBody ([], venv) = ()
		  | processBody ({name, params, result, body, pos}::decs, venv) =
		    let
			fun enterparam ({name, escape, typ, pos}, venv) =
			    let
				val ty = case S.look(tenv, typ) of
					     SOME(t) => t
					   | NONE => (error pos ("Undefined parameter type: " ^ S.name(typ));T.BOTTOM)
				val accesslevel = Tr.allocLocal level (!escape)
			    in
				S.enter(venv, name, E.VarEntry{access=accesslevel, ty=ty})
			    end
			val venv' = foldr enterparam venv params
			val {exp=bodyExp, ty=bodyType} = transExp(venv', tenv, level, donelabel) body
			val entry = case S.look(venv, name) of
					SOME(E.FunEntry en) => en
				      | _ => (ErrorMsg.impossible "Function not found")
			val _ = Tr.procEntryExit({level=(#level entry), body=bodyExp})						
		    in
			(if hasSameType(bodyType, resultTy(result), pos, pos) then
			     ()
			 else
			     (error pos "Type mismatch between function body and return type");
			 processBody(decs, venv))
		    end			
		fun checkRepeatedName(decs) =
		    let
			fun addName({name, params, result, body, pos}, set) =
			    if binarySet.member(set, S.name(name)) then
				(error pos ("Repeated function name: " ^ S.name(name)); set)
			    else
				binarySet.add(set, S.name(name))
			val allnames = List.foldr addName binarySet.empty decs
		    in
			if binarySet.numItems(allnames) = List.length(decs) then
			    false
			else
			    true				
		    end			
		(* first pass add header information to environment *)
		val venv' = List.foldr processHeader venv decs
		(* second pass function bodies *)
		val temp = processBody(decs, venv')
	    in
		if checkRepeatedName(decs) then
		    {venv=venv, tenv=tenv, explist=explist}
		else
		    {venv=venv', tenv=tenv, explist=explist}
	    end
    in
	trdec
    end	

and transTy (tenv, A.NameTy (symb, pos)) =
(*    T.NAME (symb, ref (S.look(tenv, symb)))*)

    (case S.look(tenv, symb) of
	 SOME(ty) => ty
       | NONE     => (error pos ("Undefined type " ^ S.name symb); T.UNIT))
   | transTy (tenv, A.RecordTy fields) =
    let
	fun field_type {name, escape, typ, pos} =
	    case S.look(tenv, typ) of
		SOME ty => (name, ty)
	      | NONE => (error pos ("TransTy: undefined type " ^ S.name name);
			 (name, T.UNIT))
    in
	T.RECORD (List.map field_type fields, ref ())
    end
  | transTy (tenv, A.ArrayTy (symb, pos)) =
    (case S.look(tenv, symb) of
	SOME ty => T.ARRAY (ty, ref ())
      | NONE => (error pos "Undefined array type"; T.UNIT))

fun transProg (e : A.exp) =
    let
	val _ = (Tr.fragref := [])
	val venv = Env.base_venv
	val tenv = Env.base_tenv
	val baselevel = Tr.newLevel({parent=Tr.outermost, name=Temp.namedlabel("main_function"), formals=[]})
	val donelabel = Temp.namedlabel("exit_0")				       
	val tree = transExp(venv, tenv, baselevel, donelabel) e
	val _ = Tr.procEntryExit({level=baselevel, body=(#exp tree)})	      
    in	
	 {frags=Tr.getResult(), ty=(#ty tree)}
    end
end
