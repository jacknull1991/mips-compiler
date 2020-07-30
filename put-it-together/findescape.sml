signature FINDESCAPE =
sig
    val findEscape: Absyn.exp -> unit
end

structure FindEscape:> FINDESCAPE =
struct

type depth = int
type escEnv = (depth * bool ref) Symbol.table

fun traverseVar (env:escEnv, d:depth, s:Absyn.var): unit = ()
and traverseExp (env:escEnv, d:depth, s:Absyn.exp): unit = ()
and traverseDecs (env:escEnv, d:depth, s:Absyn.dec list): escEnv = env

fun findEscape (prog: Absyn.exp): unit = ()
					     

end
