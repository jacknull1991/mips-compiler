signature MAKEGRAPH =
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph: MAKEGRAPH =
struct

structure A = Assem
structure F = Flow
structure G = Graph

fun instrs2graph instrlist =
    let
	val newGraph = G.newGraph()
			 
	(* create nodes - init def and use sets *)			 
	fun addNode (a::l) =
	    let
		val newNode = G.newNode(newGraph)
		val ({instrs, defs, uses, moves}, nodelist) = addNode(l)

		fun initNodeSets (dst, src, ismove) =
		    {instrs=G.Table.enter(instrs, newNode, a),
		     defs=G.Table.enter(defs, newNode, dst),
		     uses=G.Table.enter(uses, newNode, src),
		     moves=G.Table.enter(moves, newNode, ismove)}
	    in
		((case a of
		      A.OPER{assem, dst, src, jump} =>
		      initNodeSets(dst, src, false)				   
		    | A.LABEL{assem, lab} =>
		      initNodeSets([], [], false)
		    | A.MOVE{assem, dst, src} =>
		      initNodeSets([dst], [src], true)),
		 nodelist @ [newNode])
	    end
	  | addNode ([]) =
	    ({instrs=G.Table.empty,
	      defs=G.Table.empty,
	      uses=G.Table.empty,
	      moves=G.Table.empty}, [])
		
	val ({instrs, defs, uses, moves}, nodelist) = addNode(instrlist)
							     
	(* create edges - create edge between node n and m *)
	fun addEdges (instrs, n::(m::l)) =
	    let
		val ins_n = G.Table.look(instrs, n)
		fun createLabel (label, m::l): G.node =
		    let
			val ins_m = G.Table.look(instrs, m)
		    in
			(case ins_m of
			     SOME(A.LABEL{assem, lab}) =>
			     if label = lab then m
			     else
				 createLabel(label, l)
			   | _ => createLabel(label, l))
		    end
		  | createLabel (_,_) = m 
	    in
		(G.mk_edge({from=n, to=m});
		 (case ins_n of
		      SOME(A.OPER{assem,dst,src,jump}) =>
		      (case jump of
			   SOME(labs) => app (fn lab =>
						 G.mk_edge({from=n, to=createLabel(lab, nodelist)})) labs
			 | NONE => ())
		    | _ => ());
		 addEdges(instrs, m::l))
	    end
	  | addEdges (_,_) = () 

	val _ = (addEdges(instrs, nodelist))		    
    in
	(F.FGRAPH{control=newGraph, def=defs, use=uses, ismove=moves}, nodelist)
    end								     	    
end
