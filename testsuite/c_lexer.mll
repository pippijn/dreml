let lower	= ['a'-'z']
let upper	= ['A'-'Z']

let digit	= ['0'-'9']

let alpha	= (lower | upper)
let alnum	= (alpha | digit)

let identifier	= (alpha | '_')(alnum | '_')*

let bstring	= '`'  ('\\' _ | [^ '\\' '`' ])* '`'
let dstring	= '"'  ('\\' _ | [^ '\\' '"' ])* '"'
let sstring	= '\'' ('\\' _ | [^ '\\' '\''])* '\''


let d	= digit
let o	= ['0'-'7']
let h	= ['a'-'f' 'A'-'F' '0'-'9']
let xh	= ('0'['x''X'])
let b	= ['0' '1']
let xb	= ('0'['b''B'])
let e	= (['E''e']['+''-']?d+)
let p	= (['P''p']['+''-']?d+)
let fs	= (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is	= (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws	= [' ' '\t' '\r']

let u	= ['\x80'-'\xbf']


rule token = parse
  | identifier			as id	{ 1 }

  | (xh h+ as i) (is? as is)		{ 1 }
  | (xb b+ as i) (is? as is)		{ 1 }
  | ('0'o+ as i) (is? as is)		{ 1 }
  | (d+ as i) (is? as is)		{ 1 }

  |(d+
  | d+e
  | d*'.'d+e?
  | d+'.'d*e? as f) (fs? as fs)		{ 1 }
  |(xh h*p h*
  | xh h*'.'h*p h* as f) (fs? as fs)	{ 1 }

  | sstring			as s	{ 1 }
  | dstring			as s	{ 1 }
  | 'L'sstring			as s	{ 1 }
  | 'L'dstring			as s	{ 1 }

  | ","					{ 1 }
  | "..."				{ 1 }
  | ">>"				{ 1 }
  | ">>="				{ 1 }
  | "<<"				{ 1 }
  | "<<="				{ 1 }
  | "+"					{ 1 }
  | "+="				{ 1 }
  | "-"					{ 1 }
  | "-="				{ 1 }
  | "*"					{ 1 }
  | "*="				{ 1 }
  | "/"					{ 1 }
  | "/="				{ 1 }
  | "%"					{ 1 }
  | "%="				{ 1 }
  | "&"					{ 1 }
  | "&="				{ 1 }
  | "^"					{ 1 }
  | "^="				{ 1 }
  | "|"					{ 1 }
  | "|="				{ 1 }
  | "<"					{ 1 }
  | "<="				{ 1 }
  | ">"					{ 1 }
  | ">="				{ 1 }
  | "="					{ 1 }
  | "=="				{ 1 }
  | "!="				{ 1 }

  | "&&"				{ 1 }
  | "||"				{ 1 }
  | "++"				{ 1 }
  | "--"				{ 1 }

  | "->"				{ 1 }
  | "."					{ 1 }

  | "!"					{ 1 }
  | "~"					{ 1 }
  | ";"					{ 1 }
  | ":"					{ 1 }
  | "?"					{ 1 }

  | "("					{ 1 }
  | ")"					{ 1 }
  | "{"					{ 1 }
  | "}"					{ 1 }
  | "["					{ 1 }
  | "]"					{ 1 }

  | '#' ws*				{ 1 }

  | "%d" as wc				{ 1 }
  | "%e" as wc				{ 1 }
  | "%t" as wc				{ 1 }

  | '\n'				{ 1 }
  | ws+					{ 1 }

  | eof					{ 0 }

  | ['\xc0'-'\xdf'] u
  | ['\xe0'-'\xef'] u u
  | ['\xf0'-'\xf7'] u u u
  | ['\xf8'-'\xfb'] u u u u
  | ['\xfc'-'\xfd'] u u u u u as utf8	{ 1 }

  | _ as c				{ 1 }

{
  let slurp ic =
    let lst = ref [] in
    try while true do lst := input_line ic :: !lst done; assert false
    with End_of_file -> String.concat "\n" (List.rev !lst)

  let main =
    let fh = open_in "wip/nsHTMLEditRules.i" in
    let input = slurp fh in
    close_in fh;

    for i = 1 to 20 do
      let min_time = ref 1000.0 in

      let data = ref "" in
      for j = 1 to i do
        data := !data ^ input;
      done;
      let input = !data in

      for t = 1 to 20 do
        Gc.compact ();
	let lexbuf = Lexing.from_string input in

        let s = Unix.gettimeofday () in
	while token lexbuf <> 0 do
	  ()
	done;
        let e = Unix.gettimeofday () in

        if !min_time > (e -. s) then (
          min_time := e -. s;
          Printf.printf "%d,%06f\r" (String.length input) !min_time;
          flush stdout;
        );
      done;

      print_newline ();
    done;
}
