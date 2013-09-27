(*+ -auto-loc
 *)
{
  open Oparser

  let (|>) = BatPervasives.(|>)


  type automaton =
    | Normal
    | Verbatim

  type state = {
    code : Buffer.t;
    mutable automaton : automaton;
    mutable brace_level : int;
    mutable start_p : Lexing.position;
  }

  let make () = {
    code = Buffer.create 16;
    automaton = Normal;
    brace_level = 0;
    start_p = Lexing.dummy_pos;
  }

  let keywords = [
    "and",		TOK_AND;
    "as",		TOK_AS;
    "let",		TOK_LET;
    "parse",		TOK_PARSE;
    "rule",		TOK_RULE;
  ]

  let classify id =
    try
      snd (List.find (fun (name, token) -> Sloc.value id = name) keywords)
    with Not_found ->
      TOK_LNAME id

  let to_string = function
    | TOK_INTEGER i -> Printf.sprintf "TOK_INTEGER %d" i
    | TOK_UNAME id -> "TOK_UNAME " ^ Sloc.value id
    | TOK_LNAME id -> "TOK_LNAME " ^ Sloc.value id
    | TOK_BUILTIN id -> "TOK_BUILTIN " ^ Sloc.value id
    | TOK_CHAR id -> "TOK_CHAR " ^ Char.escaped (Sloc.value id)
    | TOK_STRING id -> "TOK_STRING " ^ Sloc.value id
    | TOK_LIT_CODE id -> "TOK_LIT_CODE " ^ Sloc.value id

    | TOK_EQUALS -> "TOK_EQUALS"
    | TOK_UNDERLINE -> "TOK_UNDERLINE"
    | TOK_EOF -> "TOK_EOF"

    | TOK_PIPE -> "TOK_PIPE"
    | TOK_CARET -> "TOK_CARET"
    | TOK_MINUS -> "TOK_MINUS"

    | TOK_PLUS -> "TOK_PLUS"
    | TOK_STAR -> "TOK_STAR"
    | TOK_QUESTION -> "TOK_QUESTION"
    | TOK_COMMA -> "TOK_COMMA"

    | TOK_LBRACE -> "TOK_LBRACE"
    | TOK_RBRACE -> "TOK_RBRACE"
    | TOK_LBRACK -> "TOK_LBRACK"
    | TOK_RBRACK -> "TOK_RBRACK"
    | TOK_LPAREN -> "TOK_LPAREN"
    | TOK_RPAREN -> "TOK_RPAREN"

    | TOK_PROPERTY -> "TOK_PROPERTY"
    | TOK_AND -> "TOK_AND"
    | TOK_AS -> "TOK_AS"
    | TOK_LET -> "TOK_LET"
    | TOK_PARSE -> "TOK_PARSE"
    | TOK_RULE -> "TOK_RULE"

    | TOK_ERROR c -> "TOK_ERROR " ^ Char.escaped (Sloc.value c)

    | EOF -> "EOF"


  let include_file inc =
    let startpos = String. index inc '"' + 1 in
    let endpos	 = String.rindex inc '"' in
    String.sub inc startpos (endpos - startpos)


  let remove_quotes str =
    String.sub str 1 (String.length str - 2)


  let remove_braces str =
    let startpos = String. index str '{' + 1 in
    let endpos	 = String.rindex str '}' in
    String.sub str startpos (endpos - startpos)
    |> BatString.trim


  let remove_parens str =
    let startpos = String. index str '(' + 1 in
    let endpos	 = String.rindex str ')' in
    String.sub str startpos (endpos - startpos)
    |> BatString.trim


  let parse_string str =
    Sloc.at str (Sloc.value str |> remove_quotes |> CoreString.unescaped)

  let parse_char str =
    let unesaped = Sloc.value (parse_string str) in
    if String.length unesaped != 1 then (
      Diagnostics.error str "invalid character literal: %s (%s)"
	(Sloc.value str)
	(String.escaped unesaped)
    );
    Sloc.at str unesaped.[0]


  let output_position out p =
    let open Lexing in
    Printf.fprintf out "(%s, %d, %d)"
      p.pos_fname
      p.pos_lnum
      (p.pos_cnum - p.pos_bol)

  let loc lexbuf ?(s=Lexing.lexeme_start_p lexbuf) t =
    let e = Lexing.lexeme_end_p lexbuf in
    (t, s, e)
}


let uident = ['A'-'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
let lident = ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*

let dstring = '"' ('\\' _ | [^ '"'  '\\' '\n'])* '"'
let sstring = "'" ('\\' _ | [^ '\'' '\\' '\n'])+ "'"

let ws = [' ' '\t' '\r']


rule verbatim state = parse
| '{'			{
			  state.brace_level <- state.brace_level + 1;
			  Buffer.add_char state.code '{';
			  verbatim state lexbuf
			}

| '}'			{
			  state.brace_level <- state.brace_level - 1;
			  Buffer.add_char state.code '}';
			  if state.brace_level = 0 then
			    let code = Buffer.contents state.code in
			    Buffer.clear state.code;
			    state.automaton <- Normal;
			    TOK_LIT_CODE (loc ~s:state.start_p lexbuf (remove_braces code))
			  else
			    verbatim state lexbuf
			}

| '\n' as c		{ Buffer.add_char state.code c; Lexing.new_line lexbuf; verbatim state lexbuf }
| [^ '{' '}' '\n' '\'' '"']+
| sstring
| dstring as s		{ Buffer.add_string state.code s; verbatim state lexbuf }


and normal state = parse
(* Whitespace *)
| '\n'					{ Lexing.new_line lexbuf; normal state lexbuf }
| ws+					{ normal state lexbuf }

(* Comments *)
| "(*"					{ comment 0 state lexbuf }

(* State-switching keywords *)

(* Identifier *)
| "eof"					{ TOK_EOF }
| '_'					{ TOK_UNDERLINE }
| "\\p{"				{ TOK_PROPERTY }
| '\\' (uident | lident) as name	{ TOK_BUILTIN (loc lexbuf name) }
| "[:" lident ":]" as name		{ TOK_BUILTIN (loc lexbuf name) }
| uident as name			{ TOK_UNAME (loc lexbuf name) }
| lident as name			{ classify (loc lexbuf name) }

(* Integer *)
| ['0'-'9']+ as int			{ TOK_INTEGER (int_of_string int) }

(* String/character *)
| dstring as string			{ TOK_STRING (loc lexbuf string |> parse_string) }
| sstring as string			{ TOK_CHAR (loc lexbuf string |> parse_char) }

(* Punctuators *)
| '{'					{ state.brace_level <- 1;
					  state.start_p <- Lexing.lexeme_start_p lexbuf;
					  Buffer.add_char state.code '{';
					  verbatim state lexbuf
					}
| '^'					{ TOK_CARET }
| '?'					{ TOK_QUESTION }
| '*'					{ TOK_STAR }
| '+'					{ TOK_PLUS }
| '|'					{ TOK_PIPE }
| '-'					{ TOK_MINUS }
| '='					{ TOK_EQUALS }
| ','					{ TOK_COMMA }
| '['					{ TOK_LBRACK }
| ']'					{ TOK_RBRACK }
| '('					{ TOK_LPAREN }
| ')'					{ TOK_RPAREN }

| _ as c				{ TOK_ERROR (loc lexbuf c) }

| eof					{ EOF }


and comment level state = parse
| [^ '(' '*' ')' '\n']+			{ comment level state lexbuf }
| "(*"					{ comment (level + 1) state lexbuf }
| "*)"					{ if level > 0 then comment (level - 1) state lexbuf else normal state lexbuf }
| '\n'					{ Lexing.new_line lexbuf; comment level state lexbuf }
| _					{ comment level state lexbuf }


{
  let token state lexbuf =
    let token =
      match state.automaton with
      | Normal   -> normal
      | Verbatim -> verbatim
    in
    let tok = token state lexbuf in
    (*if Options._trace_lexing () then*)
      (*print_endline (to_string tok);*)
    tok
}
