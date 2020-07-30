signature LIVENESS =
sig
    datatype igraph =
	     IGRAPH of {graph: Graph.graph,
			tnode: Temp.temp -> Graph.node,
			gtemp: Graph.node -> Temp.temp,
			moves: (Graph.node * Graph.node) list}

    val interferenceGraph: Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end

structure Liveness: LIVENESS =
struct

structure G = Flow.Graph
structure GT = G.Table
		   
structure ListSet = ListSetFn(struct type ord_key = Temp.temp
				     val compare = Int.compare
			      end)

datatype igraph =
	 IGRAPH of {graph: Graph.graph,
		    tnode: Temp.temp -> Graph.node,
		    gtemp: Graph.node -> Temp.temp,
		    moves: (Graph.node * Graph.node) list}

val iter = ref 0
fun listToSet (list) = ListSet.addList(ListSet.empty, list)				     

fun interferenceGraph (Flow.FGRAPH{control, def, use, ismove}) =
    let
	val _ = (iter := 0)
	val nodelist = G.nodes(control)
	val (liveInGlobal, liveOutGlobal) =
            (foldr (fn (n,(livein,liveout)) => (ref(GT.enter(!livein, n, ListSet.empty)),
						ref(GT.enter(!liveout, n, ListSet.empty))))
                   (ref(GT.empty),ref(GT.empty)) nodelist)
		
	(* compute liveness sets until fixed point reached *)				
	fun computeLiveness () =
	    let	     
		fun computeSets (node) =
		    let
			val succs = G.succ(node)
			val outs' = foldr (fn (succ, set) =>
					      let
						  val temps = case GT.look(!liveInGlobal, succ) of
								  SOME(s) => s
								| NONE => ListSet.empty
					      in
						  ListSet.union(temps, set)
					      end) ListSet.empty succs
			val uses = listToSet(case GT.look(use, node) of
						 SOME(us) => us
					       | NONE => [])
			val defs = listToSet(case GT.look(def, node) of
						 SOME(ds) => ds
					       | NONE => [])
			val ins' = ListSet.union(uses, ListSet.difference(outs', defs))
		    in
			(liveOutGlobal := GT.enter(!liveOutGlobal, node, outs');
			 liveInGlobal := GT.enter(!liveInGlobal, node, ins');			 
			 ())			    
		    end
	    in
		(List.app computeSets nodelist;
		 ())
	    end

	fun fixedPointReached (liveIn', liveOut') =
	    let
		fun compareNodeLiveSet (node) =
		    let
			val ins = case G.Table.look(!liveInGlobal, node) of
				      SOME(s) => s
				    | NONE => ListSet.empty								 
			val outs = case G.Table.look(!liveOutGlobal, node) of
				       SOME(s) => s
				     | NONE => ListSet.empty
			val ins' = case G.Table.look(liveIn', node) of
				       SOME(s) => s 
				     | NONE => ListSet.empty
			val outs' = case G.Table.look(liveOut', node) of
					SOME(s) => s
				      | NONE => ListSet.empty
		    in
			((*TextIO.output(TextIO.stdOut, ("Convergence Iteration " ^ Int.toString(!iter) ^ ":\n"));
			 TextIO.output(TextIO.stdOut, "in: " ^ (String.concatWith "," (map Temp.makestring (ListSet.listItems(ins)))) ^ "\n");
			 TextIO.output(TextIO.stdOut, "in': " ^ (String.concatWith "," (map Temp.makestring (ListSet.listItems(ins')))) ^ "\n");
			 TextIO.output(TextIO.stdOut, "out: " ^ (String.concatWith "," (map Temp.makestring (ListSet.listItems(outs)))) ^ "\n");
			 TextIO.output(TextIO.stdOut, "out': " ^ (String.concatWith "," (map Temp.makestring (ListSet.listItems(outs')))) ^ "\n");*)
			if ListSet.equal(ins, ins') andalso ListSet.equal(outs, outs') then
			    true
			else
			    false)
		    end

		fun compareList (a::l) =
		    if compareNodeLiveSet(a) = true then
			compareList(l)
		    else
			false
		  | compareList ([]) = true 
	    in
		compareList(nodelist)
	    end					    						    
		
	fun generateLivenessSets () =
	    let
		val liveIn' = foldr (fn (n, table) =>
					G.Table.enter(table, n, (case G.Table.look(!liveInGlobal, n) of
								     SOME(s) => s
								   | NONE => ListSet.empty))) G.Table.empty nodelist
		val liveOut' = foldr (fn (n, table) =>
					 G.Table.enter(table, n, (case G.Table.look(!liveOutGlobal, n) of
								      SOME(s) => s
								    | NONE => ListSet.empty))) G.Table.empty nodelist
		val _ = computeLiveness()
		val _ = (iter := !iter + 1)			    
	    in		
		if fixedPointReached(liveIn', liveOut') = true then
		    (!liveInGlobal, !liveOutGlobal)
		else
		    generateLivenessSets()
	    end
		
	val (fixedLiveIn, fixedLiveOut) = generateLivenessSets()
			       
	(* create new igraph with all temps *)							      
	val newGraph = G.newGraph()
	val tempSet = foldr (fn (node, set) =>
				let
				    val defTemps = case G.Table.look(def, node) of
							 SOME(l) => listToSet(l)
						       | NONE => ListSet.empty								     
				    val useTemps = case G.Table.look(use, node) of
						       SOME(l) => listToSet(l)
						     | NONE => ListSet.empty								   
				in
				    ListSet.union(set, ListSet.union(defTemps, useTemps)) end)
			    ListSet.empty nodelist
			    
	val templist = ListSet.listItems(tempSet)
	val newNodeList = map (fn _ => G.newNode(newGraph)) templist

	val nodeTempPairs = ListPair.zip(newNodeList, templist)
	val n2tMap = foldr (fn ((n,t), table) => G.Table.enter(table, n, t)) G.Table.empty nodeTempPairs
	val t2nMap = foldr (fn ((n,t), table) => Temp.Table.enter(table, t, n)) Temp.Table.empty nodeTempPairs
			      
	(* add intetference edges *)						   
	fun addEdgesForNode (node, moves) =
	    let
		val move = case G.Table.look(ismove, node) of
			       SOME(b) => b
			     | NONE => ErrorMsg.impossible "Error: ismove not found in flowgraph"
		val newDefTemps = case G.Table.look(def, node) of
				      SOME(ds) => ds
				    | NONE => []
		val liveTemps = case G.Table.look(fixedLiveOut, node) of
				    SOME(t) => ListSet.listItems(t)
				  | NONE => []
                fun addEdgeToLiveTemps (d) =
		    let
			val dnode = case Temp.Table.look(t2nMap, d) of
					SOME(n) => n
				      | NONE => ErrorMsg.impossible ("Internal Error: corresponding node not found for temporary " ^ Temp.makestring(d))
		    in
			app (fn t => (if d <> t then
					  G.mk_edge{from=dnode,
						    to=case Temp.Table.look(t2nMap, t) of
							   SOME(n) => n
							 | NONE => ErrorMsg.impossible ("Internal Error: corresponding node not found for temporary " ^ Temp.makestring(t))}			   
				      else
					  ())) liveTemps
		    end
	    in
		(app addEdgeToLiveTemps newDefTemps;
		 (if move = true then
		      let
			  val useTemps = case G.Table.look(use, node) of
					     SOME(us) => us
					   | NONE => []
		      in
			  if List.length(newDefTemps) = 1 andalso List.length(useTemps) = 1 then
			      let
				  val defTemp = List.hd(newDefTemps)
				  val useTemp = List.hd(useTemps)
				  val defNode = case Temp.Table.look(t2nMap, defTemp) of
						    SOME(n) => n
						  | NONE => ErrorMsg.impossible ("Internal Error: corresponding node not found for temporary " ^ Temp.makestring(defTemp))
				  val useNode = case Temp.Table.look(t2nMap, useTemp) of
						    SOME(n) => n
						  | NONE => ErrorMsg.impossible ("Internal Error: corresponding node not found for temporary " ^ Temp.makestring(useTemp))
			      in
				  if defTemp <> useTemp then
				      (G.mk_edge{from=defNode, to=useNode};
				       (defNode, useNode)::moves)
				  else
				      moves
			      end
			  else
			      moves
		      end
		  else
		      moves))
	    end

	val moves = foldr addEdgesForNode [] nodelist
    in
	(IGRAPH{graph=newGraph,
		tnode=fn t => case Temp.Table.look(t2nMap, t) of
				  SOME(n) => n
				| NONE => ErrorMsg.impossible ("Internal Error: corresponding node not found for temporary " ^ Temp.makestring(t)),		
		gtemp=fn n => case G.Table.look(n2tMap, n) of
				  SOME(t) => t
				| NONE => ErrorMsg.impossible ("Internal Error: corresponding temporary not found for node " ^ G.nodename(n)),		
		moves=moves},
	 fn n => case G.Table.look(fixedLiveOut, n) of
		     SOME(l) => ListSet.listItems(l)
		   | NONE => []
	)				 
    end				  

fun show (outstream, IGRAPH{graph, tnode, gtemp, moves}) =
    let
	val nodeToString = fn n => Temp.makestring(gtemp(n))
	fun printGraph (node) =
	    let
	    in
		 TextIO.output(outstream, ((nodeToString node) ^ "---->" ^ (String.concatWith ", " (map nodeToString (G.adj(node)))) ^ "\n"))
	    end		
    in
	(TextIO.output(outstream, ("Printing Interference Graph...\n"));
	 TextIO.output(outstream, ("node ----> interfered nodes\n"));
	 TextIO.output(outstream, "==================================================\n");
	 app printGraph (G.nodes(graph)))
    end	
    
end
    
