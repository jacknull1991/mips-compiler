(* signature provided by the book *)
signature ENV =
sig
    (* commented out, used in later phase *)
    (* type access *)    
    type ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
		      | FunEntry of {level: Translate.level,
				     label: Temp.label,
				     formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* predefined types *)
    val base_venv : enventry Symbol.table (* predifined functions *)
end

(* Implementation *)
structure Env : ENV =
struct
(* module abbreviations *)
structure S = Symbol
structure T = Types

type ty = T.ty 
datatype enventry = VarEntry of {access: Translate.access, ty: ty}
		  | FunEntry of {level: Translate.level,
				 label: Temp.label,
				 formals: ty list, result: ty}

(* initialize base_tenv contains int -> Ty.INT, string -> Ty.STRING *)
val base_tenv = S.enter(S.enter(S.empty, S.symbol "int", T.INT), S.symbol "string", T.STRING)

(* initialize base_level for the main Tiger Program *)		       
val base_level = Translate.newLevel{parent=Translate.outermost, name=Temp.namedlabel("baselevel"), formals=[]}		       

(* initialize base_vene contains Tiger predifined functions: print, flush ... *)
val base_venv =
    let
	val pre_fun : (string * Translate.level * Temp.label * ty list * ty) list =
	    [("print", base_level, Temp.namedlabel("print"), [T.STRING], T.UNIT),
	     ("flush", base_level, Temp.namedlabel("flush"), [], T.UNIT),
	     ("getchar", base_level, Temp.namedlabel("getchar"), [], T.STRING),
	     ("ord", base_level, Temp.namedlabel("ord"), [T.STRING], T.INT),
	     ("chr", base_level, Temp.namedlabel("chr"), [T.INT], T.STRING),
	     ("size", base_level, Temp.namedlabel("size"), [T.STRING], T.INT),
	     ("substring", base_level, Temp.namedlabel("substring"), [T.STRING, T.INT, T.INT], T.INT),
	     ("concat", base_level, Temp.namedlabel("concat"), [T.STRING, T.STRING], T.STRING),
	     ("not", base_level, Temp.namedlabel("not"), [T.INT], T.INT),
	     ("exit", base_level, Temp.namedlabel("exit"), [T.INT], T.UNIT)]
	(* add function f(...) to enveronment e *)
	fun add_fun ((f_name, f_level, f_label, formals_lst, result_ty), e) =
	    S.enter(e, S.symbol f_name, FunEntry {level=f_level, label=f_label, formals = formals_lst, result = result_ty})
    in
	List.foldr add_fun S.empty pre_fun
    end
end
