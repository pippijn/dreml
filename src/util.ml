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


let superscript_int i =
  BatString.fold_right (fun c str ->
    superscript_of_digit c :: str
  ) (string_of_int i) []
  |> String.concat ""


let rec string_of_regex = function
  | Choice (Epsilon, r)
  | Choice (r, Epsilon) -> string_of_regex r ^ "?"
  | Choice (r1, r2) -> "(" ^ string_of_regex r1 ^ "+" ^ string_of_regex r2 ^ ")"
  | Concat (r1, r2) -> "(" ^ string_of_regex r1 ^ string_of_regex r2 ^ ")"
  | Star r -> string_of_regex r ^ "*"
  | Repeat (r, n) -> string_of_regex r ^ superscript_int n
  | Not r -> "(¬" ^ string_of_regex r ^ ")"
  | Intersect (r1, r2) -> "(" ^ string_of_regex r1 ^ "∩" ^ string_of_regex r2 ^ ")"
  | Epsilon -> "ε"
  | Phi -> "Ø"
  | Letter l -> String.make 1 l


let rec string_of_pattern = function
  | VarBase (name, r) -> "(" ^ name ^ ":" ^ string_of_regex r ^ ")"
  | VarGroup (name, p) -> "(" ^ name ^ ":" ^ string_of_pattern p ^ ")"
  | PatIntersect (p1, p2) -> "(" ^ string_of_pattern p1 ^ "∩" ^ string_of_pattern p2 ^ ")"
  | PatChoice (p1, p2) -> "(" ^ string_of_pattern p1 ^ "+" ^ string_of_pattern p2 ^ ")"
  | PatConcat (p1, p2) -> "(" ^ string_of_pattern p1 ^ string_of_pattern p2 ^ ")"
  | PatStar p -> string_of_pattern p ^ "*"
  | PatRepeat (r, n) -> string_of_pattern r ^ superscript_int n


let reduce f = function
  | [] -> []
  | x :: xs -> List.fold_left f x xs


let rec rewrite f x =
  let x' = f x in
  if x = x' then
    x
  else
    rewrite f x'


let compare_list compare a_list b_list =
  let result = List.length a_list - List.length b_list in
  if result <> 0 then
    result
  else
    List.fold_left2 (fun result a b ->
      if result <> 0 then
        result
      else
        compare a b
    ) 0 a_list b_list


let compare_fst a b =
  compare (fst a) (fst b)

let equal_fst a b =
  fst a = fst b
