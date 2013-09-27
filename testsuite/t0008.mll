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
(* floats *)
|(d+e
| d*'.'d+e?
| d+'.'d*e?
| xh h*p h*
| xh h*'.'h*p h* as f) (fs? as s)                               { FLOAT }

(* integers *)
|(xh h+
| xb b+
| '0'o+
| d+ as i) (is? as s)                                           { INT }

(* identifier *)
| identifier as id                                              { NAME }

(* strings *)
| ('L'? as p)(sstring as c)                                     { CHAR }
| ('L'? as p)(dstring as s)                                     { STRING }

(* whitespace *)
| [' ' '\t' '\r' '\n']+                                         { WHITESPACE }

(* keywords, operators *)
| "__extension__"                                               { IGNORE }
| "("                                                           { LPAREN }
| ")"                                                           { RPAREN }
| "[" | "<:"                                                    { LBRACKET }
| "]" | ":>"                                                    { RBRACKET }
| "{" | "<%"                                                    { LBRACE }
| "}" | "%>"                                                    { RBRACE }
| "->"                                                          { ARROW }
| "::"                                                          { COLONCOLON }
| "."                                                           { DOT }
| "!"                                                           { BANG }
| "~"                                                           { TILDE }
| "+"                                                           { PLUS }
| "-"                                                           { MINUS }
| "++"                                                          { PLUSPLUS }
| "--"                                                          { MINUSMINUS }
| "&"                                                           { AND }
| "*"                                                           { STAR }
| ".*"                                                          { DOTSTAR }
| "->*"                                                         { ARROWSTAR }
| "/"                                                           { SLASH }
| "%"                                                           { PERCENT }
| "<<"                                                          { LEFTSHIFT }
| ">>"                                                          { RIGHTSHIFT }
| "<"                                                           { LESSTHAN }
| "<="                                                          { LESSEQ }
| ">"                                                           { GREATERTHAN }
| ">="                                                          { GREATEREQ }
| "=="                                                          { EQUALEQUAL }
| "!="                                                          { NOTEQUAL }
| "^"                                                           { XOR }
| "|"                                                           { OR }
| "&&"                                                          { ANDAND }
| "||"                                                          { OROR }
| "?"                                                           { QUESTION }
| ":"                                                           { COLON }
| "="                                                           { EQUAL }
| "*="                                                          { STAREQUAL }
| "/="                                                          { SLASHEQUAL }
| "%="                                                          { PERCENTEQUAL }
| "+="                                                          { PLUSEQUAL }
| "-="                                                          { MINUSEQUAL }
| "&="                                                          { ANDEQUAL }
| "^="                                                          { XOREQUAL }
| "|="                                                          { OREQUAL }
| "<<="                                                         { LEFTSHIFTEQUAL }
| ">>="                                                         { RIGHTSHIFTEQUAL }
| ","                                                           { COMMA }
| "..."                                                         { ELLIPSIS }
| ";"                                                           { SEMICOLON }

(* GNU *)
| ">?"								{ MAX }
| "<?"								{ MIN }

(* C++ comments *)
| "//" [^ '\n']*                                                { CPP_COMMENT }

(* C comments *)
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"                            { C_COMMENT }

| "#pragma" [^ '\n']+                                           { PRAGMA }

| [^ '\n']							{ INVALID }
| eof								{ EOF }
