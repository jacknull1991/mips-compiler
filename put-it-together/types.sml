structure Types =
struct
type unique = unit ref
datatype ty = RECORD of (Symbol.symbol * ty) list * unique
            | NIL
            | INT
            | STRING
            | ARRAY of ty * unique
	    | NAME of Symbol.symbol * ty option ref
            | UNIT
	    | BOTTOM

fun name(t) =
    case t of
	RECORD _ => "RECORD"
      | NIL => "NIL"
      | INT => "INT"
      | STRING => "STRING"
      | ARRAY _ => "ARRAY"
      | UNIT => "UNIT"
      | NAME _ => "NAME LABEL"
      | BOTTOM => "BOTTOM" 
end

