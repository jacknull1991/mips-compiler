structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   structure R = RegAlloc

   fun getsome (SOME x) = x

   fun getRegName alloc temp =
       case Temp.Table.look(alloc, temp) of
	   SOME(F.Reg(r)) => r
	 | NONE => F.registerName temp				  
			      
   fun emitproc out (F.PROC{body, frame}) =
       let
	   val _ = print ("emit " ^ (Symbol.name (F.name frame)) ^ "\n")
	   val stms = Canon.linearize body
	   val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	   val instrs = List.concat(map (MipsGen.codegen(frame)) stms')
	   val instrs' = F.procEntryExit2(frame, instrs)
           val {prolog, body=bodyInstr, epilog} = F.procEntryExit3(frame, instrs')
	   val (instrs'', allocation) = R.alloc(bodyInstr)
	   val format0 = Assem.format(getRegName allocation)				     
       in
	   TextIO.output(out, prolog);
	   app (fn i => TextIO.output(out, format0 i)) instrs'';
	   TextIO.output(out, epilog)
       end
     | emitproc out (F.STRING(lab,s)) = TextIO.output(out, F.string(lab,s))

   fun emitstr out (lab,s) = TextIO.output(out, F.string(lab,s))					  

   fun emitRuntime (out) =
       let
	   val rt = TextIO.openIn "runtimele.s"
	   val sys = TextIO.openIn "sysspim.s"
	   fun process (instream) = (case TextIO.inputLine instream of
					 SOME(l) => (TextIO.output(out, l); process(instream))
				       | NONE => ())
       in
	   (TextIO.output(out, "\n");
	    process(rt);
	    process(sys))
       end	   
	   
   fun genInstrs ({body, frame}) =
       let
	   val stms = Canon.linearize body
	   val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	   val instrs = List.concat(map (MipsGen.codegen(frame)) stms')
	   val instrs' = F.procEntryExit2(frame, instrs)
       in
	   instrs'
       end	   
	   
   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
       in (f out before TextIO.closeOut out) 
	      handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let
	   val absyn = Parse.parse filename
           val {frags, ty} = Semant.transProg absyn
	   val (procs, strs) = List.partition(fn x =>
						 case x of
						     F.PROC _ => true
						   | _ => false) frags					     
       in
	   TestUnit.println("=============COMPILING FILE: " ^ filename ^ "===============");
           withOpenFile (filename ^ ".s") 
	                (fn out => (emitRuntime out;
				    TextIO.output(out, ".align 4\n.data\n");
				    app (emitproc out) strs;
				    TextIO.output(out, "\n.text\n");
				    TextIO.output(out, "\n.globl main\n");
				    app (emitproc out) procs))
       end

end
