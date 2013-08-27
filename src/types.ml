module StringMap = struct
  include Map.Make(String)

  module Show_t (S : Deriving_Show.Show) = struct
    type value = S.a
    module Show_value = S

    type bindings = (string * value) list deriving (Show)

    let format f x =
      Show_bindings.format f (bindings x)

  end
end


module Show_letter = struct

  include Deriving_Show.Show_char

  let format_list f x =
    Show.format<string> f (BatString.of_list x)

end


type letter = char
type position = int deriving (Show)
type word = Pos of position * position deriving (Show)

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
  deriving (Show)

type 'label pattern =
  | VarBase of 'label * regex
  | VarGroup of 'label * 'label pattern
  | PatIntersect of 'label pattern * 'label pattern
  | PatChoice of 'label pattern * 'label pattern
  | PatConcat of 'label pattern * 'label pattern
  | PatStar of 'label pattern
  | PatRepeat of 'label pattern * int
  | PatNot of 'label pattern
  deriving (Show)

type 'label instruction =
  | Update of 'label
  | Iterate of 'label list
  | Combine of 'label instruction * 'label instruction
  deriving (Show)

type 'label env = ('label * word) list deriving (Show)
(*type env = letter list StringMap.t deriving (Show)*)

type exprset = regex list deriving (Show)
type exprsets = exprset list deriving (Show)

type 'label exprset_pat = 'label pattern list


let empty_env = []
(*let empty_env = StringMap.empty*)


module type LabelType = sig
  type t deriving (Show)

  val make : string -> t
  val rename : t -> t
  val unrename : t -> t
  val to_string : t -> string
end


module type TransitionType = sig
  type 'label transition

  module Make : functor(Lbl : LabelType) -> sig
    type t = Lbl.t transition

    val update : Lbl.t -> t
    val iterate : Lbl.t pattern -> t
    val combine : t -> t -> t

    val execute : position -> t -> Lbl.t env -> Lbl.t env

    val to_string : t -> string
  end
end
