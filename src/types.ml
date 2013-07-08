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
type word = letter list deriving (Show)

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

type pattern =
  | VarBase of string * regex
  | VarGroup of string * pattern
  | PatIntersect of pattern * pattern
  | PatChoice of pattern * pattern
  | PatConcat of pattern * pattern
  | PatStar of pattern
  | PatRepeat of pattern * int
  | PatNot of pattern
  deriving (Show)

type instruction =
  | Update of string * char
  | Iterate of pattern
  | Combine of instruction * instruction
  deriving (Show)

type env = (string * word) list deriving (Show)
(*type env = letter list StringMap.t deriving (Show)*)

type exprset = regex list deriving (Show)
type exprsets = exprset list deriving (Show)

type exprset_pat = pattern list
type exprsets_pat = (exprset_pat * instruction) list


let empty_env = []
(*let empty_env = StringMap.empty*)


module type TransitionType = sig
  type t

  val update : (string * letter) -> t
  val iterate : pattern -> t
  val combine : t -> t -> t

  val execute : t -> env -> env

  val to_string : t -> string
end
