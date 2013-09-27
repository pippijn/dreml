let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper | '$')
let alnum = (alpha | digit)

let identifier = (alpha | '_')(alnum | '_')*

let bstring = '`'  ('\\' _ | [^ '\n' '\\' '`' ])* '`'
let dstring = '"'  ('\\' _ | [^ '\n' '\\' '"' ])* '"'
let sstring = '\'' ('\\' _ | [^ '\n' '\\' '\''])* '\''


let d = digit
let o = ['0'-'7']
let h = ['a'-'f' 'A'-'F' '0'-'9']
let xh = ('0'['x''X'])
let b = ['0' '1']
let xb = ('0'['b''B'])
let e = (['E''e']['+''-']?d+)
let p = (['P''p']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws = [' ' '\t' '\r']

let u = ['\x80'-'\xbf']


rule token = parse
(* keywords, operators *)
| "__extension__"			{ IGNORE }

| "signed"
| "__signed"
| "__signed__"				{ KW_SIGNED }
| "unsigned"
| "__unsigned"
| "__unsigned__"			{ KW_UNSIGNED }
| "_Bool"				{ KW_BOOL }
| "char"				{ KW_CHAR }
| "short"				{ KW_SHORT }
| "int"					{ KW_INT }
| "long"				{ KW_LONG }
| "float"				{ KW_FLOAT }
| "double"				{ KW_DOUBLE }
| "void"				{ KW_VOID }

| "const"
| "__const"
| "__const__"				{ KW_CONST }
| "volatile"
| "__volatile"
| "__volatile__"			{ KW_VOLATILE }
| "restrict"
| "__restrict"
| "__restrict__"			{ KW_RESTRICT }

| "auto"				{ KW_AUTO }
| "extern"				{ KW_EXTERN }
| "inline"				{ KW_INLINE }
| "__inline"
| "__inline__"				{ KW_INLINE }
| "register"				{ KW_REGISTER }
| "static"				{ KW_STATIC }
| "__thread"				{ KW_THREAD }
| "typedef"				{ KW_TYPEDEF }

| "enum"				{ KW_ENUM }
| "struct"				{ KW_STRUCT }
| "union"				{ KW_UNION }
| "__fc_datatype"			{ KW_DATATYPE }

| "break"				{ KW_BREAK }
| "case"				{ KW_CASE }
| "continue"				{ KW_CONTINUE }
| "default"				{ KW_DEFAULT }
| "do"					{ KW_DO }
| "else"				{ KW_ELSE }
| "for"					{ KW_FOR }
| "goto"				{ KW_GOTO }
| "if"					{ KW_IF }
| "return"				{ KW_RETURN }
| "sizeof"				{ KW_SIZEOF }
| "switch"				{ KW_SWITCH }
| "while"				{ KW_WHILE }

| "_Complex"
| "__complex"
| "__complex__"				{ KW_COMPLEX }
| "__alignof"
| "__alignof__"				{ KW_ALIGNOF }
| "asm"
| "__asm"
| "__asm__"				{ KW_ASM }
| "__builtin_offsetof"			{ KW_OFFSETOF }
| "__builtin_types_compatible_p"	{ KW_TYPES_COMPATIBLE_P }
| "__builtin_va_arg"			{ KW_VA_ARG }
| "__builtin_va_list"
| "__builtin_ms_va_list"
| "__builtin_sysv_va_list"		{ KW_VA_LIST }
| "__extension__"			{ KW_EXTENSION }
| "_fastcall"
| "__fastcall"				{ KW_FASTCALL }
| "__imag"
| "__imag__"				{ KW_IMAG }
| "__label__"				{ KW_LABEL }
| "__real"
| "__real__"				{ KW_REAL }
| "typeof"
| "__typeof"
| "__typeof__"				{ KW_TYPEOF }

(* floats *)
|(d+e
| d*'.'d+e?
| d+'.'d*e?
| xh h*p h*
| xh h*'.'h*p h* as f) (fs? as s)	{ FLOAT }

(* integers *)
|(xh h+
| xb b+
| '0'o+
| d+ as i) (is? as s)			{ INT }

(* identifier *)
| identifier as id			{ NAME }

(* strings *)
| ('L'? as p)(sstring as c)		{ CHAR }
| ('L'? as p)(dstring as s)		{ STRING }

(* whitespace *)
| [' ' '\t' '\r' '\n']+			{ WHITESPACE }

(* operators *)
| "("					{ LPAREN }
| ")"					{ RPAREN }
| "[" | "<:"				{ LBRACKET }
| "]" | ":>"				{ RBRACKET }
| "{" | "<%"				{ LBRACE }
| "}" | "%>"				{ RBRACE }
| "->"					{ ARROW }
| "::"					{ COLONCOLON }
| "."					{ DOT }
| "!"					{ BANG }
| "~"					{ TILDE }
| "+"					{ PLUS }
| "-"					{ MINUS }
| "++"					{ PLUSPLUS }
| "--"					{ MINUSMINUS }
| "&"					{ AND }
| "*"					{ STAR }
| ".*"					{ DOTSTAR }
| "->*"					{ ARROWSTAR }
| "/"					{ SLASH }
| "%"					{ PERCENT }
| "<<"					{ LEFTSHIFT }
| ">>"					{ RIGHTSHIFT }
| "<"					{ LESSTHAN }
| "<="					{ LESSEQ }
| ">"					{ GREATERTHAN }
| ">="					{ GREATEREQ }
| "=="					{ EQUALEQUAL }
| "!="					{ NOTEQUAL }
| "^"					{ XOR }
| "|"					{ OR }
| "&&"					{ ANDAND }
| "||"					{ OROR }
| "?"					{ QUESTION }
| ":"					{ COLON }
| "="					{ EQUAL }
| "*="					{ STAREQUAL }
| "/="					{ SLASHEQUAL }
| "%="					{ PERCENTEQUAL }
| "+="					{ PLUSEQUAL }
| "-="					{ MINUSEQUAL }
| "&="					{ ANDEQUAL }
| "^="					{ XOREQUAL }
| "|="					{ OREQUAL }
| "<<="					{ LEFTSHIFTEQUAL }
| ">>="					{ RIGHTSHIFTEQUAL }
| ","					{ COMMA }
| "..."					{ ELLIPSIS }
| ";"					{ SEMICOLON }

(* GNU *)
| ">?"					{ MAX }
| "<?"					{ MIN }

(* C++ comments *)
| "//" [^ '\n']*			{ CPP_COMMENT }

(* C comments *)
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"	{ C_COMMENT }

| "#pragma" [^ '\n']+			{ PRAGMA }

| [^ '\n']				{ INVALID }
| eof					{ EOF }
