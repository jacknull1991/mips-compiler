signature TESTUNIT =
sig
    val println: string -> unit
end

structure TestUnit:> TESTUNIT =
struct
fun println (out) = (TextIO.output(TextIO.stdOut, out ^ "\n"))

end
