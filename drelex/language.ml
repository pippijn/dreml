open Types


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

  | _ -> failwith "nullable not computed"


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

  | _ -> failwith "nullable not computed"


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
