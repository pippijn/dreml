open Types

(*
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
      *)


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


let rec is_empty_language_pat = function
  | VarBase (x, r) ->
      is_empty_language r
  | VarGroup (x, p) ->
      is_empty_language_pat p

  | PatStar _ -> false
  | PatRepeat (r, n) -> assert (n > 0); is_empty_language_pat r
  | PatChoice (r1, r2) -> is_empty_language_pat r1 && is_empty_language_pat r2
  | PatConcat (r1, r2) -> is_empty_language_pat r1 || is_empty_language_pat r2
  | PatIntersect (r1, r2) -> is_empty_language_pat r1 || is_empty_language_pat r2
  | PatNot p -> not_is_empty_language_pat p

and not_is_empty_language_pat = function
  | VarBase (x, r) ->
      not_is_empty_language r
  | VarGroup (x, p) ->
      not_is_empty_language_pat p

  | PatStar _ -> false

  (* De Morgan *)
  | PatIntersect (r1, r2) -> not_is_empty_language_pat r1 || not_is_empty_language_pat r2
  | PatChoice (r1, r2) -> not_is_empty_language_pat r1 && not_is_empty_language_pat r2

  (* Double negation *)
  | PatNot r -> is_empty_language_pat r

  | PatRepeat (r, n) -> assert (n > 0); not_is_empty_language_pat r

  (* XXX: correct? *)
  | PatConcat (a, b) -> not_is_empty_language_pat a && not_is_empty_language_pat b


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


let rec nullable_pat = function
  | VarBase (x, r) ->
      nullable r
  | VarGroup (x, p) ->
      nullable_pat p

  | PatStar _ -> true

  (* e.g. a | a* is nullable *)
  | PatChoice (p1, p2) -> nullable_pat p1 || nullable_pat p2
  (* e.g. a & a* is not nullable *)
  | PatIntersect (p1, p2)
  (* e.g. a a* is not nullable *)
  | PatConcat (p1, p2) -> nullable_pat p1 && nullable_pat p2
  | PatRepeat (p, n) -> assert (n > 0); nullable_pat p

  | PatNot (p) ->
      not (nullable_pat p)
