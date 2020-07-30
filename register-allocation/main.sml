structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame
   (* structure R = RegAlloc *)

   fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
       let
	   val _ = print ("emit " ^ (Symbol.name (F.name frame)) ^ "\n")
	   val stms = Canon.linearize body
           val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	   val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
           val instrs2 = F.procEntryExit2(frame, instrs)
           val {prolog, body = bodyInstrs, epilog} = F.procEntryExit3(frame, instrs2)
           val format0 = Assem.format(Temp.makestring)
       in
           app (fn i => TextIO.output(out,format0 i)) instrs2;
           (*app (fn i => TextIO.output(TextIO.stdOut,format0 i)) instrs2;*)
	   RegAlloc.alloc(instrs2);
	   ()
       end
     | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
       in (f out before TextIO.closeOut out) 
	      handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let
	   val absyn = Parse.parse filename
           val {frags, ty} = Semant.transProg absyn
					      
       in
	   TestUnit.println("Compiling " ^ filename ^ "...");
	   TestUnit.println("\nPrinting assembly instructions...");
           withOpenFile (filename ^ ".s") 
	                    (fn out => (app (emitproc out) frags))
       end

end
