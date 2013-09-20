open CorePervasives
open Types

let not3 = function
  | No -> Yes
  | Yes -> No
  | Maybe -> failwith "Maybe in not3"

let (|||) a b =
  match a, b with
  | No, No -> No
  | _, Yes
  | Yes, _ -> Yes
  | _, Maybe
  | Maybe, _ -> failwith "Maybe in |||"

let (&&&) a b =
  match a, b with
  | Yes, Yes -> Yes
  | No, _
  | _, No -> No
  | _, Maybe
  | Maybe, _ -> failwith "Maybe in &&&"


let rec exprset_of_regex = function
  | Intersect (_, r1, r2) -> exprset_of_regex r1 @ exprset_of_regex r2
  | r -> [r]

let rec exprsets_of_regex = function
  | Choice (_, r1, r2) -> exprsets_of_regex r1 @ exprsets_of_regex r2
  | r -> [exprset_of_regex r]


let rec regex_of_exprset = function
  | [] -> failwith "empty exprset" (* Phi? *)
  | [r] -> r
  | r1 :: rs ->
      let r2 = regex_of_exprset rs in
      Intersect (
        Language.nullable r1 &&& Language.nullable r2,
        r1, r2
      )

let rec pattern_of_exprset = function
  | [] -> failwith "empty exprset" (* Phi? *)
  | [p] -> p
  | p1 :: ps ->
      let p2 = pattern_of_exprset ps in
      PatIntersect (
        Language.nullable_pat p1 &&& Language.nullable_pat p2,
        p1, p2
      )


let set_union a b =
  let table = Hashtbl.create (List.length a + List.length b) in

  let unite =
    List.fold_left (fun union x ->
      if Hashtbl.mem table x then (
        union
      ) else (
        Hashtbl.add table x ();
        x :: union
      )
    )
  in

  let union = unite (unite [] a) b in

  List.rev union


let set_union_pat a b =
  let table = ExprsetTbl.create (List.length a + List.length b) in

  let unite =
    List.fold_left (fun union x ->
      if ExprsetTbl.mem table x then (
        union
      ) else (
        ExprsetTbl.add table x ();
        x :: union
      )
    )
  in

  let union = unite (unite [] a) b in

  List.rev union


let set_union = (@)
let set_union_pat = (@)


let rec mapx1 f x1 = function
  | [] -> []
  | a :: l ->
      f x1 a :: mapx1 f x1 l

(*
let rec rev_mapx1 accu f x1 = function
  | [] -> accu
  | a :: l ->
      rev_mapx1 (f x1 a :: accu) f x1 l
*)


let rec mapx2 f x1 x2 = function
  | [] -> []
  | a :: l ->
      f x1 x2 a :: mapx2 f x1 x2 l

(*
let rec rev_mapx2 accu f x1 x2 = function
  | [] -> accu
  | a :: l ->
      rev_mapx2 (f x1 x2 a :: accu) f x1 x2 l
*)


(* circled · *)
let mul_exprsets_expr ess g =
  let fun_2 g e =
    Concat (Language.nullable e &&& Language.nullable g, e, g)
  in
  let fun_1 g es =
    mapx1 fun_2 g es
  in
  mapx1 fun_1 g ess

let mul_exprsets_expr_pat p1ss p2 =
  let fun_2 p2 p1 =
    PatConcat (Language.nullable_pat p1 &&& Language.nullable_pat p2, p1, p2)
  in
  let fun_1 p2 (p1s, f) =
    (mapx1 fun_2 p2 p1s, f)
  in
  mapx1 fun_1 p2 p1ss

let mul_exprsets_expr_pat_iter iterate p1ss p2 =
  let fun_2 p2 p1 =
    PatConcat (Language.nullable_pat p1 &&& Language.nullable_pat p2, p1, p2)
  in
  let fun_1 p2 iterate (p1s, f) =
    (mapx1 fun_2 p2 p1s, Tag.compose f iterate)
  in
  mapx2 fun_1 p2 iterate p1ss


(* circled ∩ *)
let intersect_exprsets ess fss =
  let fun_1 fss es =
    (* for each F set, combine with the E set *)
    mapx1 set_union es fss
  in
  let union =
    (* for each E set *)
    mapx1 fun_1 fss ess
  in
  (* unite every es with every fs *)
  Util.reduce set_union union


let intersect_exprsets_pat ess fss =
  let fun_2 (es, e_tag) (fs, f_tag) =
    (* combine the two sets and associated
     * transition functions *)
    set_union es fs, Tag.compose e_tag f_tag
  in
  let fun_1 fss es =
    (* for each F set *)
    mapx1 fun_2 es fss
  in
  let union =
    (* for each E set *)
    mapx1 fun_1 fss ess
  in
  (* unite every es with every fs *)
  Util.reduce set_union_pat union


(* circled ¬ *)
let not_exprsets sets =
  let fun_2 e =
    [Not (not3 (Language.nullable e), e)]
  in
  let fun_1 set =
    List.map fun_2 set
  in
  Util.reduce intersect_exprsets (
    List.map fun_1 sets
  )


(*
let filter_valid =
  List.filter (not % List.exists (Simplify.simplify %> Language.is_empty_language))

let simplify =
  List.map (List.map Simplify.simplify)
*)


(* Definition 2. Partial derivative as sets of expression sets. *)
let rec derive l = function
  (* 2 *)
  | Letter l2 when l = l2 -> [[Epsilon]]
  (* 1 *)
  (* the paper says ∅ (empty set) here, but we would have to special-case
     empty set in other functions, so we make it the set containing just
     the set with the empty language symbol *)
  | Phi
  | Epsilon
  | Letter _ (* otherwise *) -> [[Phi]]
  (* 3 *)
  | Choice (_, r1, r2) ->
      let r1' = derive l r1 in
      let r2' = derive l r2 in
      set_union r1' r2'
  (* 4 *)
  | Star (r1) as star ->
      mul_exprsets_expr (derive l r1) star
  (* 5 *)
  | Concat (_, r1, r2) ->
      let r1' = derive l r1 in
      let r1'r2 = mul_exprsets_expr r1' r2 in
      if Language.nullable r1 = Yes then
        set_union
          r1'r2
          (derive l r2)
      else
        r1'r2
  (* 6 *)
  | Not (_, r) -> not_exprsets (derive l r)
  (* 7 *)
  | Intersect (_, r1, r2) ->
      intersect_exprsets (derive l r1) (derive l r2)
  (* additional *)
  | Repeat (_, r, 1) ->
      derive l r
  | Repeat (null, r, n) ->
      assert (n > 1);
      assert (null != Maybe);
      derive l (Concat (null, r, Repeat (null, r, n - 1)))


(* ·\p· :: l -> p -> [p] *)
let rec derive_pat l p =
  List.map (fun (p, t) ->
    List.map Simplify.simplify_pat p, t
  )
  begin match p with
  | VarBase (_, x, r) ->
      derive_VarBase l x r
  | VarGroup (_, x, p) ->
      derive_VarGroup l x p
  | PatChoice (_, p1, p2) ->
      derive_PatChoice l p1 p2
  | PatConcat (_, p1, p2) ->
      derive_PatConcat l p1 p2
  | PatIntersect (_, p1, p2) ->
      derive_PatIntersect l p1 p2
  | PatStar (p) as star ->
      derive_PatStar l p star
  | PatRepeat (_, p, 1) ->
      derive_PatRepeat1 l p
  | PatRepeat (null, p, n) ->
      derive_PatRepeat l null p n
  | PatNot _ ->
      failwith "Negation in pattern must be resolved before derivation"
  end

and derive_VarBase l x r =
  let f = Tag.update x in
  mapx2 (fun x f r ->
    let r' = regex_of_exprset r in
    [VarBase (Language.nullable r', x, r')], f
  ) x f (derive l r)

and derive_VarGroup l x p =
  let update = Tag.update x in
  mapx2 (fun x update (p', f) ->
    let p' = pattern_of_exprset p' in
    [VarGroup (Language.nullable_pat p', x, p')],
    Tag.compose update f
  ) x update (derive_pat l p)

and derive_PatChoice l p1 p2 =
  set_union
    (derive_pat l p1)
    (derive_pat l p2)

and derive_PatConcat l p1 p2 =
  let p1' = derive_pat l p1 in
  let p1'p2 = mul_exprsets_expr_pat p1' p2 in
  if Language.nullable_pat p1 = Yes then
    set_union_pat
      p1'p2
      (derive_pat l p2)
  else
    p1'p2

and derive_PatIntersect l p1 p2 =
  intersect_exprsets_pat
    (derive_pat l p1)
    (derive_pat l p2)

and derive_PatStar l p star =
  let iterate = Tag.iterate p in
  mul_exprsets_expr_pat_iter iterate (derive_pat l p) star

and derive_PatRepeat1 l p =
  let iterate = Tag.iterate p in
  mapx1 (fun iterate (p', f) ->
    p', Tag.compose f iterate
  ) iterate (derive_pat l p)

and derive_PatRepeat l null p n =
  assert (n > 1);
  let iterate = Tag.iterate p in
  let repeat = PatRepeat (null, p, n - 1) in
  mul_exprsets_expr_pat_iter iterate (derive_pat l p) repeat


let derive_pat l p =
  let fun_1 (p, f) =
    pattern_of_exprset p, f
  in
  List.map fun_1 (derive_pat l p)
