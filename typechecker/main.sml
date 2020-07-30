structure Main =
struct
fun main filename =
    let
	val e = Parse.parse filename
    in
	Semant.transProg e
    end	
end
