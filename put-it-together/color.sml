signature COLOR =
sig
    structure F: FRAME
    type allocation = F.register Temp.Table.table
    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: Graph.node -> int,
		registers: F.register list}
	       -> allocation * Temp.temp list					 
end

structure Color: COLOR =
struct

structure F = MipsFrame
structure G = Flow.Graph
structure T = Temp

type allocation = F.register T.Table.table

structure RegSet = BinarySetFn(struct type ord_key = F.register
				      fun compare (F.Reg(r1), F.Reg(r2)) = String.compare(r1, r2)
			       end)

structure TempSet = BinarySetFn(struct type ord_key = T.temp
				       val compare = Int.compare
				end)				    

fun listToSet (l) = G.Set.addList(G.Set.empty, l)

fun printColors ([]) = (TextIO.output(TextIO.stdOut, "\n"))
  | printColors (a::l) =
    (TextIO.output(TextIO.stdOut, F.registerName(a) ^ " ");
     printColors(l))

fun printNodeList ([]) = TextIO.output(TextIO.stdOut, "\n")
  | printNodeList (a::l) =
    let
	val name = G.nodename(a)
    in
	(TextIO.output(TextIO.stdOut, name ^ " ");
	 printNodeList(l))
    end			     
			       
fun color ({interference=Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers}) =
    let
	val nodes = G.nodes(graph)
	val colorMap = ref initial: allocation ref
	(* Number of colors *)			
	val K = List.length(registers)
			   
	val (precolored, uncolored) = List.partition (fn x => (case T.Table.look(initial, gtemp(x)) of
								   SOME(t) => true
								 | NONE => false)) nodes
	(* helper functions *)						     
	fun makeTable ((node, value), table) = G.Table.enter(table, node, value)
	fun numOfAdj (node) = List.length(G.adj(node))

	(* adjacency list representation of the graph *)
	val adjList = foldr makeTable G.Table.empty (ListPair.zipEq(uncolored, (map G.adj uncolored)))
	(* a table containing the current degree of each node *)			    
	val degree = ref (foldr makeTable G.Table.empty (ListPair.zipEq(uncolored, (map numOfAdj uncolored))))

	val simplifyWorklist = ref (listToSet(List.filter (fn n => numOfAdj(n) < K) uncolored))
	val spillWorklist = ref (listToSet(List.filter (fn n => numOfAdj(n) >= K) uncolored))

	val spilledNodes = ref []
	val coloredNodes = ref []
	val selectStack = ref []

	val colorSet = ref (TempSet.addList(TempSet.empty, F.colorList))
	(*val availableColors = colorSet*)
	val color = ref T.Table.empty			

	(* adjacent nodes not on stack *)			      
	fun adjacent (node) =
	    let
		val adjNodes = case G.Table.look(adjList, node) of
				   SOME(ns) => listToSet(ns)
				 | NONE => G.Set.empty
		val selected = listToSet(!selectStack)
	    in
		G.Set.difference(G.Set.difference(adjNodes, selected), listToSet(precolored))
	    end

	(* decrement degree of the node, remove if less than K *)		
	fun decrementDegree (n) =
	    let
		val deg = case G.Table.look(!degree, n) of
			      SOME(d) => d
			    | NONE => ErrorMsg.impossible "Internal error: couldn't find the degree of node"
	    in
		degree := G.Table.enter(!degree, n, deg-1);
		(if deg = K then
		     (spillWorklist := G.Set.delete(!spillWorklist, n);
		      simplifyWorklist := G.Set.add(!simplifyWorklist, n))
		 else ())
	    end

	(* simplify graph, remove node and add to stack *)		
	fun simplify () =
	    let
		val node = List.hd(G.Set.listItems(!simplifyWorklist))
	    in
		(simplifyWorklist := G.Set.delete(!simplifyWorklist, node);
		 selectStack := node :: !selectStack;
		 G.Set.app decrementDegree (adjacent(node)))
	    end

	fun coalesce () = () (* TODO: coalesce move edge *)

	fun freezeMoves (node) = () (* TODO: unfreeze move edge to allow more simplification *)
			 
	fun selectSpill () =
	    let
		(* TODO: select node with lowest cost to spill *)
		val node = List.hd(G.Set.listItems(!spillWorklist))
	    in
		(spillWorklist := G.Set.delete(!spillWorklist, node);
		 simplifyWorklist := G.Set.add(!simplifyWorklist, node);
		 freezeMoves(node))		    
	    end

	fun assignColors () =
	    (case !selectStack of
		 [] => ()
	       | a::l =>
		 let
		     val availableColors = ref (TempSet.addList(TempSet.empty, F.colorList))
		     val coloredSet = listToSet(!coloredNodes)
		     val adjNodes = case G.Table.look(adjList, a) of
					SOME(ns) => ns
				      | NONE => []
		     fun removeColor ([]) = ()
		       | removeColor (n::rest) =
			 (if G.Set.member(coloredSet, n) = true then
			      let
				  val c = case T.Table.look(!color, gtemp(n)) of
					      SOME(co) => co
					    | NONE => ErrorMsg.impossible ("Couldn'n find color of node " ^ G.nodename(n))
			      in
				  (if TempSet.member(!availableColors, c) = true then
				       (availableColors := TempSet.delete(!availableColors, c);
					removeColor(rest))
				   else
				       ())
			      end
			  else
			      removeColor(rest))
		 in
		     (removeColor(adjNodes);
		      (if TempSet.isEmpty(!availableColors) = false then
			   let
			       val newcolor = List.hd(TempSet.listItems(!availableColors))
			       val newnode = gtemp(a)
			   in
			       (selectStack := l;
				coloredNodes := a :: !coloredNodes;
				colorMap := T.Table.enter(!colorMap, newnode, F.getReg(newcolor));
				color := T.Table.enter(!color, newnode, newcolor))
			   end
		       else
			   (ErrorMsg.impossible "Internal error: not enough registers. Implement spilling."));
		      assignColors())
		 end)			  
	fun repeat () =
	    (if not (G.Set.isEmpty(!simplifyWorklist)) then
		 (simplify();
		  repeat())
	     else if not (G.Set.isEmpty(!spillWorklist)) then
		 (selectSpill();
		  repeat())
	     else
		 ())

	val spills = map gtemp (!spilledNodes)
    in
	TestUnit.println("Coloring graph...");
	repeat();
	assignColors();
	(!colorMap, spills)
    end
	
end
    
		  
    
