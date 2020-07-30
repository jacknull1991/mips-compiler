structure Semant:
	  sig
	      val transProg : Absyn.exp -> unit
	  end	      
=
struct
(* abbreviations *)	       
structure A = Absyn
structure S = Symbol
structure T = Types
structure E = Env
		  
type venv = Env.enventry S.table
type tenv = T.ty S.table
type expty = {exp: Translate.exp, ty: T.ty} 
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
			  
fun checkInt ({exp = _, ty = t}, pos) =
    case t of T.INT => ()
	     | _ => error pos "integer required"

fun checkUnit ({exp = _, ty = t}, pos) =
    case t of T.UNIT => ()
	    | _ => error pos "unit required"
			  
fun checkString ({exp = _, ty = t}, pos) =
    case t of T.STRING => ()
	     | _ => error pos "string required"

fun checkRecord ({exp = _, ty = t}, pos) =
    case t of T.RECORD _ => ()
	    | T.NIL => ()
	    | _ => error pos "record required"

fun checkArray ({exp = _, ty = t}, pos) =
    case t of T.ARRAY _=> ()
	     | _ => error pos "array required"			  

(* to be implemented, skip NAMES? *)			  
(* fun actual_ty (ty : T.ty) = ty *)
fun actual_ty (ty:T.ty, pos) =
    case ty of
	T.NAME (symb, tyref) =>
	(case !tyref of
             NONE => (error pos ("actual_ty: undefined type " ^ S.name(symb)); T.NIL)
	   | SOME t => actual_ty (t, pos))
      | T.ARRAY(t, unique) => T.ARRAY(actual_ty(t, pos), unique)
      | _ => ty
	       
fun lookupType (tenv, ty, pos) =
    case S.look(tenv, ty) of
	NONE => (error pos ("Type " ^ S.name ty ^ " not defined"); T.NIL)
      | SOME ty => actual_ty(ty, pos)

(*fun transTy (tenv, ty = NameTy (symb, pos)) =
	    T.NAME (symb, ref (S.look(tenv, symb)))			    
*)
fun transExp (venv, tenv) =
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
	  | trexp (A.NilExp) = {exp = (), ty = T.NIL} 
	  | trexp (A.IntExp _) = {exp = (), ty = T.INT}
	  | trexp (A.StringExp _) = {exp = (), ty = T.STRING}
	  | trexp (A.CallExp{func, args, pos}) =
	    (case S.look(venv, func) of
		NONE => (error pos "function undefined";
			 {exp = (), ty = T.NIL})
	      | SOME (E.VarEntry _) =>
		(error pos "function expected but variable found";
		 {exp = (), ty = T.NIL})
	      | SOME (E.FunEntry {formals, result}) =>
		if (List.length args <> List.length formals) then
		    (error pos "number of args not equal parameters";
		     {exp = (), ty = T.NIL})
		else
		    let
			val arg_forms = ListPair.zipEq(args, formals)
			fun check_exp_ty (arg, form) =
			    if
				form <> #ty (trexp arg)
			    then
				(error pos "param type mismatch with arg type";
				 ())
			    else
				()
		    in
			(List.app check_exp_ty arg_forms;
			 {exp = (), ty = actual_ty(result, pos)})
		    end
	    )
	  | trexp (A.OpExp{left, oper, right, pos}) =
	    (case classOp oper of
		ArithOp =>
		(checkInt(trexp left, pos);
		 checkInt(trexp right, pos);
		 {exp = (), ty = T.INT})
 	      | CmpOp =>
		(case trexp left of
		     {exp = _, ty = T.INT} =>
		     (checkInt(trexp right, pos);
		      {exp = (), ty = T.INT})
		   | {exp = _, ty = T.STRING} =>
		     (checkString(trexp right, pos);
		      {exp = (), ty = T.INT})
		   | _ =>
		     (error pos "int or string required";
		      {exp = (), ty = T.INT}))
	      | EqNeqOp =>
		(case trexp left of
		     {exp = _, ty = T.INT} =>
		     (checkInt(trexp right, pos);
		      {exp = (), ty = T.INT})
		   | {exp = _, ty = T.STRING} =>
		     (checkString(trexp right, pos);
		      {exp = (), ty = T.INT})		 
		   | {exp = _, ty = T.RECORD _} =>		     		     
		     (checkRecord(trexp right, pos);
		      {exp = (), ty = T.INT})
		   | {exp = _, ty = T.ARRAY _} =>		     
		     (checkArray(trexp right, pos);
		      {exp = (), ty = T.INT})
		   | _ => 
		     (error pos "int or string required";
		      {exp = (), ty = T.INT})))
	  | trexp (A.RecordExp {fields, typ, pos}) =
	    (case S.look(tenv, typ) of
		NONE =>  (error pos "record type not found";
			  {exp = (), ty = T.NIL})
	      | SOME t' =>
		
		let val t  = actual_ty (t', pos)
		in
		    (case t of
			 T.RECORD (rec_lst, unique) =>
			 let
			     fun checkField ((symbol, exp, pos), (symb, ty)) =
				 if actual_ty(ty, pos) = #ty (trexp exp) then
				     ()
				 else if actual_ty(ty, pos) = T.NIL then
				     (case #ty (trexp exp) of
					  T.RECORD(fields, unique) => ()
					| _ => (error pos ("field not in record type: " ^ S.name symbol); ()))
				 else if #ty (trexp exp) = T.NIL then
				     (case actual_ty(ty, pos) of
					  T.RECORD(fields, unique) => ()
					| _ => (error pos ("field not in record type: " ^ S.name symbol); ()))
				 else
				     (error pos ("field not in record type: " ^ S.name symbol); ())

			     fun checkFields rec_lst =
				 if List.length rec_lst <> List.length fields then
				     (error pos "number of record fields not correct"; ())
				 else
				     List.app checkField (ListPair.zipEq(fields, rec_lst))
			 in
			     (checkFields rec_lst;
			      {exp = (), ty = t})
			 end
		       | _ => (error pos "not record type";
			       {exp = (), ty = T.NIL}))
		end
	    )
		
	  | trexp (A.SeqExp exps) =
		{exp = (), ty = if List.null exps then
				    T.UNIT
				else
				    #ty (List.last (List.map trexp (List.map #1 exps)))}
	  | trexp (A.AssignExp {var, exp, pos}) =
	    let
		val var_ty = actual_ty(#ty (trvar var), pos)
		val exp_ty = #ty (trexp exp)
	    in
		if var_ty = exp_ty then
		    {exp = (), ty = var_ty}
		else if exp_ty = T.NIL then
		    (case var_ty of
			 T.RECORD(fields, unique) => {exp = (), ty = var_ty}
		       | _ => (error pos "LHS type mismatch with RHS type"; {exp = (), ty = T.NIL}))
		else
		    {exp = (), ty = T.NIL}
	    end
	  | trexp (A.IfExp {test, then', else' = NONE, pos}) =
	    (checkInt(trexp test, pos);
	     checkUnit(trexp then', pos);
	     {exp = (), ty = T.UNIT})
	  | trexp (A.IfExp {test, then', else' = SOME else'', pos}) =
	    let
		val then_ty = #ty (trexp then')
		val else_ty = #ty (trexp else'')
	    in
		(checkInt(trexp test, pos);
		 if then_ty = else_ty then
		     {exp = (), ty = then_ty}
		 else
		     (error pos "different type in then and else";
		      {exp = (), ty = T.NIL}))
	    end
	  | trexp (A.WhileExp {test, body, pos}) =
	    (breakable := true;
	     looplevel := !looplevel + 1;
	     checkInt (trexp test, pos);
	     checkUnit (trexp body, pos);
	     looplevel := !looplevel - 1;
	     {exp = (), ty = T.UNIT})
	  | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
	    let
		val venv' = S.enter(venv, var, E.VarEntry {ty = T.INT})
	    in
		(breakable := true;
		 looplevel := !looplevel + 1;
		 checkInt (trexp lo, pos);
		 checkInt (trexp hi, pos);
		 checkUnit (transExp (venv', tenv) body, pos);
		 looplevel := !looplevel - 1;
		 {exp = (), ty = T.UNIT})
	    end
	  | trexp (A.BreakExp pos) =
	    if (!looplevel = 0) then
		(error pos "break found outside of loop"; {exp = (), ty = T.UNIT})
	    else if !breakable = false then
		(error pos "duplicated break found"; {exp = (), ty = T.UNIT})
	    else
		(breakable := false; {exp = (), ty = T.UNIT})
	  | trexp (A.LetExp{decs, body, pos}) =
	    let
		val {venv = venv', tenv = tenv'} =
(*		    List.foldl (fn (d, {v, t}) =>
				   transDec(v, t, d))
			       {venv = venv, tenv = tenv} decs
*)
 		    transDecs(venv, tenv, decs)
	    in
		transExp(venv', tenv') body
	    end
	  | trexp (A.ArrayExp {typ, size, init, pos}) =
	    (case S.look(tenv, typ) of
		 NONE =>  (error pos "array type not found";
			   {exp = (), ty = T.NIL})
	       | SOME t =>
		 (case actual_ty(t, pos) of
		      T.ARRAY (ty, unique) =>
		      (checkInt(trexp size, pos);
		       if ty = #ty (trexp init) then (* TODO: replace with helper function *)
			   {exp = (), ty = t}
		       else if #ty (trexp init) = T.NIL then
			   (case ty of
				T.RECORD(fields, unique) => {exp = (), ty = t}
			      | _ => (error pos "array type mismatched"; {exp = (), ty = T.NIL}))
		       else
			   (error pos "array type mismatched"; {exp = (), ty = T.NIL}))
(*		       if ty <> #ty (trexp init) then
			   (error pos "array type mismatched";
			    {exp = (), ty = T.NIL})
		       else
			   {exp = (), ty = t})*)
		    | _ => (error pos "not array type";
			    {exp = (), ty = T.NIL})))
	and trvar (A.SimpleVar(id, pos)) =
	    (case S.look(venv, id)
	      of SOME(E.VarEntry {ty}) => {exp = (), ty = actual_ty(ty,pos)}
	       | SOME(E.FunEntry _) => (error pos ("expected var but found fun");
					{exp = (), ty = T.INT})
	       | NONE => (error pos ("undefined variable " ^ S.name id);
			  {exp = (), ty = T.INT}))
	  | trvar (A.FieldVar (v, id, pos)) =
	    let
		val {exp, ty} = trvar v
	    in
		case actual_ty(ty, pos) of
		    T.RECORD (fields, unique) =>
		    (case List.find (fn (s, t) => s = id) fields of
			 NONE => (error pos ("field not exist " ^ S.name id);
				  {exp = (), ty = T.NIL})
		       | SOME (s, t) => {exp = (), ty = actual_ty(t, pos)})
		  | _ => (error pos "variable not a record";
			  {exp = (), ty = T.NIL})
	    end
	  | trvar (A.SubscriptVar (v, id, pos)) =
	    let
		val {exp, ty} = trvar v
	    in
		case actual_ty(ty, pos) of
		    T.ARRAY (tyArr, unique) =>
		    let
			val {exp, ty} = trexp id
		    in
			case ty of
			    T.INT => {exp = (), ty = tyArr}
			  | _ => (error pos "subscript should be int";
				  {exp = (), ty = T.NIL})
		    end
		  | _ => (error pos "array expected";
			  {exp = (), ty = T.NIL})
	    end
    in
	trexp
    end
and transDecs (venv, tenv, decs) =
    List.foldl (fn (d, {venv = v, tenv = t}) => transDec(v, t, d))
	       {venv = venv, tenv = tenv} decs
(*
and transDecs (venv, tenv, []) = {venv = venv, tenv = tenv}
  | transDecs (venv, tenv, hd::tl) =
    let
	val {venv = venv', tenv = tenv'} = transDec(venv, tenv, hd)
    in
	transDecs(venv', tenv', tl)
    end
*)
(* transDec of VarDec *)
and transDec (venv, tenv, A.VarDec {name, escape, typ = NONE, init, pos}) =
    let
	val {exp, ty} = transExp(venv, tenv) init
    in
	{venv = S.enter(venv, name, E.VarEntry{ty = ty}),
	 tenv = tenv}
    end
  | transDec (venv, tenv, A.VarDec {name, escape, typ = SOME (symb, varDecPos), init, pos}) =
    let
	val {exp, ty} = transExp(venv, tenv) init
    in
	if
	     actual_ty (ty, pos) = lookupType(tenv, symb, varDecPos) (* TODO: replace with helper function *)
	then
	    {venv = S.enter(venv, name, E.VarEntry{ty = ty}),
	     tenv = tenv}
	else if actual_ty (ty, pos) = T.NIL then
	    (case lookupType(tenv, symb, varDecPos) of
		T.RECORD(fields, unique) => {venv = S.enter(venv, name, E.VarEntry{ty = lookupType(tenv, symb, varDecPos)}), tenv = tenv}
	      | _ => (error pos ("declared type " ^ S.name symb ^ " mismatched with exp type"); {venv = venv, tenv = tenv}))
	else
	    (error pos ("declared type " ^ S.name symb ^ " mismatched with exp type");
	     {venv = venv, tenv = tenv})
    end
  (* TypeDec: generalized to handle list *)
  | transDec (venv, tenv, A.TypeDec decs) =				       
    let
	val tenv' = List.foldr (fn ({name, ty, pos}, env) =>
				   S.enter(env, name, T.NAME(name, ref NONE)))
			       tenv decs
	val tenv'' = List.foldr (fn ({name, ty, pos},env) =>
				    (case S.look(env, name) of
					 SOME(T.NAME(n,r)) =>
					 (r := SOME(transTy(env,ty)); env)
				       | _ => (error pos "mutually recursive type not found"; env))) tenv' decs
	(* check if there is cycle starting with a given type *)			
	fun findCycle(name:A.symbol, ty:T.ty, pos:A.pos, repeated) =
	    case ty of
		T.NAME(sym, tyref) =>
		if (name=sym andalso repeated=true)
		then (error pos ("illegal cycle in declaration: " ^ S.name(sym)); true)
		else
		    (case !tyref of
			SOME(t) => findCycle(name, t, pos, true)
		      | NONE => (error pos "type not found in checking cycle"; false))
	      | _ => false
	(* check if there is cycle in declaration list *)		 
	fun checkCycle ([]) = false
	  | checkCycle ({name, ty, pos}::types) =
	    case S.look(tenv'', name) of
		SOME(t) => if findCycle(name, t, pos, false) then true else checkCycle(types)
	      | NONE => (error pos "type not found in checking cycle"; true)
	fun checkRepeatedName(decs) =
	    let
		fun addName({name, ty, pos}, set) =
		    binarySet.add(set, S.name name)
		val allnames = List.foldr addName binarySet.empty decs
	    in
		if binarySet.numItems(allnames) = List.length(decs) then
		    false
		else
		    (error 0 "repeated type name found"; true)
	    end
    in
	if (checkCycle(decs) orelse checkRepeatedName(decs)) then {venv = venv, tenv = tenv} else {venv = venv, tenv = tenv''}
    end
	
  (* generalized to handle multiple functions and also procedures *)
  | transDec (venv, tenv, A.FunctionDec decs) =
    let
	fun resultTy res =
	    case res of
		NONE => T.UNIT
	      | SOME (rt, pos) =>
		(case S.look(tenv, rt) of
		     NONE => (error pos "return type undefined";
			      T.UNIT)
		   | SOME(t) => t)
	(* add params and return type *)
	fun processHeader ({name, params, result, body, pos}, venv) =
	    let
		val res_ty = resultTy result
		fun transparam {name, escape, typ, pos} =
		    case S.look(tenv, typ) of
			SOME(t) => {name = name, ty = t}
		      | NONE => (error pos "param type undefined"; {name = name, ty = T.NIL})
		val params' = List.map transparam params
	    in
		S.enter(venv, name, E.FunEntry{formals = List.map #ty params', result = res_ty})
	    end
	(* handle function body *)		
	fun processBody ([], venv) = ()
	  | processBody ({name, params, result, body, pos}::decs, venv) =
	    let
		fun enterparam ({name, escape, typ, pos}, venv) =
		    let
			val ty = case S.look(tenv, typ) of
				     SOME(t) => t
				   | NONE => (error pos ("undefined type of parameter: " ^ S.name typ);T.NIL)
		    in
			S.enter(venv, name, E.VarEntry{ty = ty})
		    end			
		val venv' = List.foldr enterparam venv params
	    in
		(* trans function body and check return type *)
		(case resultTy result of
		     T.INT => checkInt(transExp(venv', tenv) body, pos)
		   | T.STRING => checkString(transExp(venv', tenv) body, pos)
		   | T.UNIT => checkUnit(transExp(venv', tenv) body, pos)
		   | T.ARRAY(ty, unique) => checkArray(transExp(venv', tenv) body, pos)
		   | T.RECORD(fields, unique) => checkRecord(transExp(venv', tenv) body, pos)					    
		   | _ => error pos "illegal function return type";
		 processBody(decs, venv))
	    end
	(* first pass add header information to environment *)
	val venv' = List.foldr processHeader venv decs
	(* second pass function bodies *)			       
	val temp = processBody(decs, venv')
	fun checkRepeatedName(decs) =
	    let
		fun addName({name, params, result, body, pos}, set) =
		    binarySet.add(set, (S.name name))
		val allnames = List.foldr addName binarySet.empty decs
	    in
		if binarySet.numItems(allnames) = List.length(decs) then
		    false
		else
		    (error 0 "repeated function name found"; true)
	    end
    in
	if checkRepeatedName(decs) then
	    {venv = venv, tenv = tenv}
	else
	    {venv = venv', tenv = tenv}	    
    end		

and transTy (tenv, A.NameTy (symb, pos)) =
(*    T.NAME (symb, ref (S.look(tenv, symb)))*)

    (case S.look(tenv, symb) of
	 SOME(ty) => ty
       | NONE     => (error pos ("undefined type " ^ S.name symb); T.UNIT))
   | transTy (tenv, A.RecordTy fields) =
    let
	fun field_type {name, escape, typ, pos} =
	    case S.look(tenv, typ) of
		SOME ty => (name, ty)
	      | NONE => (error pos ("transTy: undefined type " ^ S.name name);
			 (name, T.UNIT))
    in
	T.RECORD (List.map field_type fields, ref ())
    end
  | transTy (tenv, A.ArrayTy (symb, pos)) =
    (case S.look(tenv, symb) of
	SOME ty => T.ARRAY (ty, ref ())
      | NONE => (error pos "undefined array type"; T.UNIT))

fun transProg (e : A.exp) : unit =
    (transExp (Env.base_venv, Env.base_tenv) e;
     ())
end
