open Types


let rec is_empty_language = function
  | Phi -> true
  | Epsilon
  | Star _
  | Letter _ -> false
  | Repeat (r, n) -> assert (n > 0); is_empty_language r
  | Choice (r1, r2) -> is_empty_language r1 && is_empty_language r2
  | Concat (r1, r2) -> is_empty_language r1 || is_empty_language r2
  | Intersect (r1, r2) -> is_empty_language r1 || is_empty_language r2
  | Not p -> not_is_empty_language p

and not_is_empty_language = function
  | Epsilon
  | Star _
  | Letter _
  | Phi -> false

  (* De Morgan *)
  | Intersect (r1, r2) -> not_is_empty_language r1 || not_is_empty_language r2
  | Choice (r1, r2) -> not_is_empty_language r1 && not_is_empty_language r2

  (* Double negation *)
  | Not r -> is_empty_language r

  | Repeat (r, n) -> assert (n > 0); not_is_empty_language r

  (* XXX: correct? *)
  | Concat (a, b) -> not_is_empty_language a && not_is_empty_language b


let rec nullable = function
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
  | PatNot (p) ->
      Not (regex_of_pattern p)
