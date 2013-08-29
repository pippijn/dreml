type letter = char

type position = {
  start_p : int;
  end_p   : int;
}

type 'a label =
  | Name of string
  | Fun of (position -> 'a)
  | NumberedFun of (position -> 'a) * int


type regex =
  | Intersect of regex * regex
  | Choice of regex * regex
  | Concat of regex * regex
  | Star of regex
  | Repeat of regex * int
  | Not of regex
  | Epsilon
  | Phi
  | Letter of letter

type 'a pattern =
  | VarBase      : 'a label * regex -> 'a pattern
  | VarGroup     : ('a -> 'b) label * 'a pattern -> 'b pattern
  | PatIntersect : 'a pattern * 'a pattern -> 'a pattern
  | PatChoice    : 'a pattern * 'a pattern -> 'a pattern
  | PatConcat    : 'a pattern * 'b pattern -> ('a * 'b) pattern
  | PatStar      : 'a pattern -> 'a pattern
  | PatRepeat    : 'a pattern * int -> 'a pattern
  | PatNot       : 'a pattern -> 'a pattern


type env

type 'a exprset = 'a pattern list
type 'a exprsets = ('a exprset * (env -> env)) list
