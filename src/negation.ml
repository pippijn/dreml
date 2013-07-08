open Types

(* *)
let rec normal_form = function
  | PatNot (VarBase (x, r)) ->
      VarBase (x, Not r)
  | PatNot (VarGroup (x, p)) ->
      VarGroup (x, normal_form (PatNot p))

  (* Double negation *)
  | PatNot (PatNot (p)) ->
      normal_form p

  (* De Morgan *)
  | PatNot (PatIntersect (p1, p2)) ->
      PatChoice (normal_form (PatNot p1), normal_form (PatNot p2))
  | PatNot (PatChoice (p1, p2)) ->
      PatIntersect (normal_form (PatNot p1), normal_form (PatNot p2))

  (* Undefined *)
  | PatNot (PatConcat (p1, p2)) ->
      failwith "don't know yet: PatNot (PatConcat _ _)"
  | PatNot (PatStar p) ->
      failwith "don't know yet: PatNot (PatStar _)"
  | PatNot (PatRepeat (p, n)) ->
      failwith "don't know yet: PatNot (PatRepeat _ _)"

  (* Recursive simplify *)
  | VarBase _ as r -> r
  | VarGroup (x, p) -> VarGroup (x, normal_form p)

  | PatIntersect (p1, p2) -> PatIntersect (normal_form p1, normal_form p2)
  | PatConcat (p1, p2) -> PatConcat (normal_form p1, normal_form p2)
  | PatChoice (p1, p2) -> PatChoice (normal_form p1, normal_form p2)
  | PatStar p -> PatStar (normal_form p)
  | PatRepeat (p, n) -> PatRepeat (normal_form p, n)
