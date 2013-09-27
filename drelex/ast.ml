open Sexplib.Conv


type chr  = char   Sloc.t with sexp
type str  = string Sloc.t with sexp
type name = string Sloc.t with sexp
type code = string Sloc.t with sexp


type property =
  | NameProperty of name option * name
  | IntProperty of name * int
  with sexp


type range =
  | Single of chr             		(* ['a'] *)
  | Range of chr * chr           	(* ['a'-'z'] *)
  with sexp

type char_class =
  | Positive of range list		(* ['a' 'b'] *)
  | Negative of range list		(* [^ 'a' 'b'] *)
  with sexp


type regexp =
  (* atoms *)
  | Eof                                 (* eof *)
  | AnyChar                             (* _ *)
  | Char of chr               		(* 'c' *)
  | String of str             		(* "class" *)
  | Lexeme of name                      (* reference to let-defined lexeme *)
  | Sequence of regexp list             (* sub-regexps in parenthesis *)
  | Alternation of regexp list		(* sub-regexps separated by "|" *)
  | CharClass of char_class	        (* character class *)
  | CharProperty of property		(* unicode property *)
  (* modifiers *)
  | Question of regexp                  (* regexp? *)
  | Star of regexp                      (* regexp* *)
  | Plus of regexp                      (* regexp+ *)
  | Quantified of regexp
                * int option
                * int option            (* regexp{1,5} *)
  (* as-binding *)
  | Binding of regexp * name
  with sexp


type alias =
  | Alias of name * regexp		(* let-defined lexeme *)
  with sexp

type rule =
  | Rule of regexp * code
  with sexp

type lexer =
  | Lexer of name * name list * rule list
  with sexp

type t =
  | Program of code option * alias list * lexer list * code option
  with sexp


let epsilon = Sequence []
