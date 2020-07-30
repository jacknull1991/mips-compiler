signature REG_ALLOC =
sig
    structure F: FRAME
    type allocation = F.register Temp.Table.table
    val alloc: Assem.instr list -> Assem.instr list * Color.allocation
end

structure RegAlloc:> REG_ALLOC =
struct
structure F = MipsFrame
structure T = Temp
structure C = Color

type allocation = F.register Temp.Table.table

fun rewriteProgram (instrs) = instrs (* TODO *)
			     
fun alloc (instrs: Assem.instr list) =
    let
	val (fgraph, nodes) = MakeGraph.instrs2graph(instrs)
	val (igraph, liveout) = Liveness.interferenceGraph(fgraph)
	(*val _ = (Liveness.show(TextIO.stdOut, igraph))*)    
	val (colorallocation, spills) = Color.color({interference=igraph,
						initial=C.F.tempMap,
						spillCost = fn _ => 1,
						registers = C.F.registers})
    in
	(if List.length(spills) <> 0 then
	    (TextIO.output(TextIO.stdOut, "Internal error: not enough registers. Implement spilling...\n");
	     (instrs, colorallocation))
	 else
	     (instrs, colorallocation)
	)
    end

end
    
	
