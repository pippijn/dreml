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
  | Repeat (_, r, n) -> assert (n > 0); is_empty_language r
  | Choice (_, r1, r2) -> is_empty_language r1 && is_empty_language r2
  | Concat (_, r1, r2) -> is_empty_language r1 || is_empty_language r2
  | Intersect (_, r1, r2) -> is_empty_language r1 || is_empty_language r2
  | Not (_, p) -> not_is_empty_language p

and not_is_empty_language = function
  | Epsilon
  | Star _
  | Letter _
  | Phi -> false

  (* De Morgan *)
  | Intersect (_, r1, r2) -> not_is_empty_language r1 || not_is_empty_language r2
  | Choice (_, r1, r2) -> not_is_empty_language r1 && not_is_empty_language r2

  (* Double negation *)
  | Not (_, r) -> is_empty_language r

  | Repeat (_, r, n) -> assert (n > 0); not_is_empty_language r

  | Concat (_, a, b) -> false


let rec is_empty_language_pat = function
  | VarBase (_, x, r) ->
      is_empty_language r
  | VarGroup (_, x, p) ->
      is_empty_language_pat p

  | PatStar _ -> false
  | PatRepeat (_, r, n) -> assert (n > 0); is_empty_language_pat r
  | PatChoice (_, r1, r2) -> is_empty_language_pat r1 && is_empty_language_pat r2
  | PatConcat (_, r1, r2) -> is_empty_language_pat r1 || is_empty_language_pat r2
  | PatIntersect (_, r1, r2) -> is_empty_language_pat r1 || is_empty_language_pat r2
  | PatNot (_, p) -> not_is_empty_language_pat p

and not_is_empty_language_pat = function
  | VarBase (_, x, r) ->
      not_is_empty_language r
  | VarGroup (_, x, p) ->
      not_is_empty_language_pat p

  | PatStar _ -> false

  (* De Morgan *)
  | PatIntersect (_, r1, r2) -> not_is_empty_language_pat r1 || not_is_empty_language_pat r2
  | PatChoice (_, r1, r2) -> not_is_empty_language_pat r1 && not_is_empty_language_pat r2

  (* Double negation *)
  | PatNot (_, r) -> is_empty_language_pat r

  | PatRepeat (_, r, n) -> assert (n > 0); not_is_empty_language_pat r

  (* XXX: correct? *)
  | PatConcat (_, a, b) -> not_is_empty_language_pat a && not_is_empty_language_pat b


let nullable = function
  | Not ((Yes | No as null), _)
  | Choice ((Yes | No as null), _, _)
  | Intersect ((Yes | No as null), _, _)
  | Concat ((Yes | No as null), _, _)
  | Repeat ((Yes | No as null), _, _) ->
      null

  | Epsilon
  | Star _ -> Yes

  | Phi
  | Letter _ -> No

  | r -> failwith (Print.string_of_regex r)


let nullable_pat = function
  | PatStar _ -> Yes

  | VarBase ((Yes | No as null), _, _)
  | VarGroup ((Yes | No as null), _, _)
  | PatChoice ((Yes | No as null), _, _)
  | PatIntersect ((Yes | No as null), _, _)
  | PatConcat ((Yes | No as null), _, _)
  | PatRepeat ((Yes | No as null), _, _)
  | PatNot ((Yes | No as null), _) ->
      null

  | _ -> assert false


let rec compute_nullable = function
  | Epsilon -> Epsilon, Yes
  | Star p -> Star (fst (compute_nullable p)), Yes

  | Phi
  | Letter _ as p -> p, No

  | Not (Maybe, r) ->
      let r, null = compute_nullable r in
      let null = not3 null in
      Not (null, r), null

  (* e.g. a | a* is nullable *)
  | Choice (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 ||| null2 in
      Choice (null, r1, r2), null

  (* e.g. a & a* is not nullable *)
  | Intersect (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 &&& null2 in
      Intersect (null, r1, r2), null

  (* e.g. a a* is not nullable *)
  | Concat (Maybe, r1, r2) ->
      let r1, null1 = compute_nullable r1 in
      let r2, null2 = compute_nullable r2 in
      let null = null1 &&& null2 in
      Concat (null, r1, r2), null

  | Repeat (Maybe, r, n) ->
      assert (n > 0);
      let r, null = compute_nullable r in
      assert (null != Maybe);
      Repeat (null, r, n), null

  | Not ((Yes | No as null), _)
  | Choice ((Yes | No as null), _, _)
  | Intersect ((Yes | No as null), _, _)
  | Concat ((Yes | No as null), _, _)
  | Repeat ((Yes | No as null), _, _) as p ->
      p, null


let rec compute_nullable_pat = function
  | VarBase (Maybe, x, r) ->
      let r, null = compute_nullable r in
      VarBase (null, x, r), null
  | VarGroup (Maybe, x, p) ->
      let p, null = compute_nullable_pat p in
      VarGroup (null, x, p), null

  | PatStar p -> PatStar (fst (compute_nullable_pat p)), Yes

  (* e.g. a | a* is nullable *)
  | PatChoice (Maybe, p1, p2) ->
      let p1, null1 = compute_nullable_pat p1 in
      let p2, null2 = compute_nullable_pat p2 in
      let null = null1 ||| null2 in
      PatChoice (null, p1, p2), null

  (* e.g. a & a* is not nullable *)
  | PatIntersect (Maybe, p1, p2) ->
      let p1, null1 = compute_nullable_pat p1 in
      let p2, null2 = compute_nullable_pat p2 in
      let null = null1 &&& null2 in
      PatIntersect (null, p1, p2), null

  (* e.g. a a* is not nullable *)
  | PatConcat (Maybe, p1, p2) ->
      let p1, null1 = compute_nullable_pat p1 in
      let p2, null2 = compute_nullable_pat p2 in
      let null = null1 &&& null2 in
      PatConcat (null, p1, p2), null

  | PatRepeat (Maybe, p, n) ->
      assert (n > 0);
      let p, null = compute_nullable_pat p in
      PatRepeat (null, p, n), null

  | PatNot (Maybe, p) ->
      let p, null = compute_nullable_pat p in
      let null = not3 null in
      PatNot (null, p), null

  | VarBase ((Yes | No as null), _, _)
  | VarGroup ((Yes | No as null), _, _)
  | PatChoice ((Yes | No as null), _, _)
  | PatIntersect ((Yes | No as null), _, _)
  | PatConcat ((Yes | No as null), _, _)
  | PatRepeat ((Yes | No as null), _, _)
  | PatNot ((Yes | No as null), _) as p ->
      p, null


let compute_nullable r = fst (compute_nullable r)
let compute_nullable_pat r = fst (compute_nullable_pat r)
