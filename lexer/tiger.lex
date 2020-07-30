type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val currStr : string ref = ref ""
val level : int ref = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in if !level <> 0 then (ErrorMsg.error pos "unclosed comment") else (); Tokens.EOF(pos,pos) end

%%
%s COMMENT STRING;
digit = [0-9];
identifier = [a-zA-Z][a-zA-Z0-9_]*;
ctrlChar = [A-Z@\[\]\\\^_];
formatChar = [\ \t\f\n\r];

%%
<INITIAL,COMMENT>\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL,COMMENT>\t => (continue());

<INITIAL>"/*" => (level := 1; YYBEGIN COMMENT; continue());
<COMMENT>"/*" => (level := !level + 1; continue());
<COMMENT>"*/" => (level := !level - 1; if !level = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>. => (continue());

<INITIAL>\" => (currStr := ""; YYBEGIN STRING; continue());
<STRING>\" => (YYBEGIN INITIAL; Tokens.STRING(!currStr,yypos-(size (!currStr)),yypos));
<STRING>\\n|\\t|\\\"|\\\\|\\{digit}{3} => (currStr := !currStr ^ (case (String.fromString yytext) of SOME c => c | NONE => yytext); continue());
<STRING>\\{formatChar}+\\ => (continue());
<STRING>\\\^{ctrlChar} => (continue());
<STRING>\\ => (ErrorMsg.error yypos ("illegal use of escape character " ^ yytext); continue());
<STRING>[\n\r] => (ErrorMsg.error yypos ("illegal string: missing end quote"); continue());
<STRING>. => (currStr := !currStr ^ yytext; continue());

<INITIAL>"type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>"var" => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>"of" => (Tokens.OF(yypos, yypos+2));
<INITIAL>"end" => (Tokens.END(yypos, yypos+3));
<INITIAL>"in" => (Tokens.IN(yypos, yypos+2));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL>"let" => (Tokens.LET(yypos, yypos+3));
<INITIAL>"do" => (Tokens.DO(yypos, yypos+2));
<INITIAL>"to" => (Tokens.TO(yypos, yypos+2));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL>"if" => (Tokens.IF(yypos, yypos+2));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL>{identifier} => (Tokens.ID(yytext, yypos, yypos+(size yytext)));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"." => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>" " => (linePos := yypos :: !linePos; continue());
<INITIAL>{digit}+ => (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos+(size yytext)));
. => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
