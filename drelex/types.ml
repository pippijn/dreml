type letter = char

type position = Pos of int * int


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

type 'label pattern =
  | VarBase      of 'label * regex
  | VarGroup     of 'label * 'label pattern
  | PatIntersect of 'label pattern * 'label pattern
  | PatChoice    of 'label pattern * 'label pattern
  | PatConcat    of 'label pattern * 'label pattern
  | PatStar      of 'label pattern
  | PatRepeat    of 'label pattern * int
  | PatNot       of 'label pattern


type env = (int * position) list

type 'label exprset = 'label pattern list
type 'label exprsets = ('label exprset * (int -> env -> env)) list

type 'label instruction =
  | Update of 'label
  | Iterate of 'label list
  | Compose of 'label instruction * 'label instruction


module ExprsetTbl = Hashtbl.Make(struct

  type t = (int exprset * int instruction)

  let equal (a, _) (b, _) =
    a = b

  let hash (a, _) =
    Hashtbl.hash a

end)
