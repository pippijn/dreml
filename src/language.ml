open Types


let rec is_empty_language r =
  let result =
    match r with
    | Phi -> true
    | Epsilon
    | Star _
    | Letter _ -> false
    | Repeat (r, n) -> assert (n > 0); is_empty_language r
    | Choice (r1, r2) -> is_empty_language r1 && is_empty_language r2
    | Concat (r1, r2) -> is_empty_language r1 || is_empty_language r2
    | Intersect (r1, r2) -> is_empty_language r1 || is_empty_language r2

    | Not (Epsilon)
    | Not (Star _)
    | Not (Letter _)
    | Not (Phi) -> false

    (* De Morgan *)
    | Not (Intersect (r1, r2)) -> is_empty_language (Choice (Not r1, Not r2))
    | Not (Choice (r1, r2)) -> is_empty_language (Intersect (Not r1, Not r2))

    (* Double negation *)
    | Not (Not r) -> is_empty_language r

    | Not (Repeat (r, n)) -> assert (n > 0); is_empty_language (Not r)

    (* XXX: correct? *)
    | Not (Concat (a, b)) -> is_empty_language (Not a) && is_empty_language (Not b)
  in
  (*
  Printf.printf "!! %-20s %s the empty language\n"
    (Util.string_of_regex r)
    (if result then "is" else "is [0;32mNOT[0m");
  *)
  result


let rec nullable r =
  let result =
    match r with
    | Epsilon
    | Star _ -> true

    | Not r -> not (nullable r)

    | Phi
    | Letter _ -> false

    (* e.g. a | a* is nullable *)
    | Choice (r1, r2) -> nullable r1 || nullable r2
    (* e.g. a & a* is not nullable *)
    | Intersect (r1, r2)
    (* e.g. a a* is not nullable *)
    | Concat (r1, r2) -> nullable r1 && nullable r2
    | Repeat (r, n) -> assert (n > 0); nullable r
  in
  (*
  Printf.printf "?? %-20s %s nullable\n"
    (Util.string_of_regex r)
    (if result then "is" else "is [0;33mNOT[0m");
  *)
  result


let rec regex_of_pattern = function
  | VarBase (x, r) ->
      r
  | VarGroup (x, p) ->
      regex_of_pattern p
  | PatIntersect (p1, p2) ->
      Intersect (regex_of_pattern p1, regex_of_pattern p2)
  | PatChoice (p1, p2) ->
      Choice (regex_of_pattern p1, regex_of_pattern p2)
  | PatConcat (p1, p2) ->
      Concat (regex_of_pattern p1, regex_of_pattern p2)
  | PatStar (p) ->
      Star (regex_of_pattern p)
  | PatRepeat (p, n) ->
      Repeat (regex_of_pattern p, n)
