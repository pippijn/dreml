type letter = char

type position = Pos of int * int

type tribool =
  | No
  | Yes
  | Maybe

let not3 = function
  | No -> Yes
  | Yes -> No
  | Maybe -> Maybe

let (|||) a b =
  match a, b with
  | No, No -> No
  | _, Yes
  | Yes, _ -> Yes
  | _, Maybe
  | Maybe, _ -> Maybe

let (&&&) a b =
  match a, b with
  | Yes, Yes -> Yes
  | No, _
  | _, No -> No
  | _, Maybe
  | Maybe, _ -> Maybe

type regex =
  | Intersect of tribool * regex * regex
  | Choice of tribool * regex * regex
  | Concat of tribool * regex * regex
  | Star of regex
  | Repeat of tribool * regex * int
  | Not of tribool * regex
  | Epsilon
  | Phi
  | Letter of letter

type 'label pattern =
  | VarBase      of tribool * 'label * regex
  | VarGroup     of tribool * 'label * 'label pattern
  | PatIntersect of tribool * 'label pattern * 'label pattern
  | PatChoice    of tribool * 'label pattern * 'label pattern
  | PatConcat    of tribool * 'label pattern * 'label pattern
  | PatStar      of 'label pattern
  | PatRepeat    of tribool * 'label pattern * int
  | PatNot       of tribool * 'label pattern


type env = (int * position) list
let empty_env : env = []

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
