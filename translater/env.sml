(* signature provided by the book *)
signature ENV =
sig
    (* commented out, used in later phase *)
    (* type access *)    
    type ty
    datatype enventry = VarEntry of {ty: ty}
		      | FunEntry of {formals: ty list, result: ty}
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
datatype enventry = VarEntry of {ty: ty}
		  | FunEntry of {formals: ty list, result: ty}

(* initialize base_tenv contains int -> Ty.INT, string -> Ty.STRING *)
val base_tenv = S.enter(S.enter(S.empty, S.symbol "int", T.INT), S.symbol "string", T.STRING)

(* initialize base_vene contains Tiger predifined functions: print, flush ... *)
val base_venv =
    let
	val pre_fun : (string * ty list * ty) list =
	    [("print", [T.STRING], T.UNIT),
	     ("flush", [], T.UNIT),
	     ("getchar", [], T.STRING),
	     ("ord", [T.STRING], T.INT),
	     ("chr", [T.INT], T.STRING),
	     ("size", [T.STRING], T.INT),
	     ("substring", [T.STRING, T.INT, T.INT], T.INT),
	     ("concat", [T.STRING, T.STRING], T.STRING),
	     ("not", [T.INT], T.INT),
	     ("exit", [T.INT], T.UNIT)]
	(* add function f(...) to enveronment e *)
	fun add_fun ((f_name, formals_lst, result_ty), e) =
	    S.enter(e, S.symbol f_name, FunEntry {formals = formals_lst, result = result_ty})
    in
	List.foldl add_fun S.empty pre_fun
    end
end
