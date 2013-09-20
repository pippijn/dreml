open CorePervasives
open Types


let superscript_of_digit = function
  | '0' -> "⁰"
  | '1' -> "¹"
  | '2' -> "²"
  | '3' -> "³"
  | '4' -> "⁴"
  | '5' -> "⁵"
  | '6' -> "⁶"
  | '7' -> "⁷"
  | '8' -> "⁸"
  | '9' -> "⁹"
  | _ -> assert false


let subscript_of_digit = function
  | '0' -> "₀"
  | '1' -> "₁"
  | '2' -> "₂"
  | '3' -> "₃"
  | '4' -> "₄"
  | '5' -> "₅"
  | '6' -> "₆"
  | '7' -> "₇"
  | '8' -> "₈"
  | '9' -> "₉"
  | _ -> assert false


let transform_int f i =
  BatString.fold_right (fun c str ->
    f c :: str
  ) (string_of_int i) []
  |> String.concat ""


let superscript_int = transform_int superscript_of_digit
let subscript_int = transform_int subscript_of_digit


let rec string_of_regex = function
  | Choice (_, Epsilon, r)
  | Choice (_, r, Epsilon) -> string_of_regex r ^ "?"
  | Choice (_, r1, r2) -> "(" ^ string_of_regex r1 ^ "+" ^ string_of_regex r2 ^ ")"
  | Concat (_, r1, r2) -> string_of_regex r1 ^ string_of_regex r2
  | Star r -> "(" ^ string_of_regex r ^ ")*"
  | Repeat (_, r, n) -> string_of_regex r ^ superscript_int n
  | Not (_, r) -> "(¬" ^ string_of_regex r ^ ")"
  | Intersect (_, r1, r2) -> "(" ^ string_of_regex r1 ^ "∩" ^ string_of_regex r2 ^ ")"
  | Epsilon -> "ε"
  | Phi -> "Ø"
  | Letter l -> String.make 1 l


let string_of_pattern string_of_label =
  let rec string_of_pattern = function
    | VarBase (_, f, r) ->
        "(" ^ string_of_label f ^ ":" ^ string_of_regex r ^ ")"
    | VarGroup (_, f, p) ->
        "(" ^ string_of_label f ^ ":" ^ string_of_pattern p ^ ")"
    | PatIntersect (_, p1, p2) ->
        string_of_pattern p1 ^ "∩" ^ string_of_pattern p2
    | PatConcat (_, p1, p2) ->
        string_of_pattern p1 ^ string_of_pattern p2
    | PatChoice (_, p1, p2) ->
        string_of_pattern p1 ^ "+" ^ string_of_pattern p2
    | PatStar (p) ->
        "(" ^ string_of_pattern p ^ ")*"
    | PatRepeat (_, p, n) ->
        string_of_pattern p ^ superscript_int n
    | PatNot (_, p) ->
        "(¬" ^ string_of_pattern p ^ ")"
  in
  string_of_pattern
