open CorePervasives
open Types


let rec exprset_of_regex = function
  | Intersect (r1, r2) -> exprset_of_regex r1 @ exprset_of_regex r2
  | r -> [r]

let rec exprsets_of_regex = function
  | Choice (r1, r2) -> exprsets_of_regex r1 @ exprsets_of_regex r2
  | r -> [exprset_of_regex r]


let rec regex_of_exprset = function
  | [] -> failwith "empty exprset" (* Phi? *)
  | [r] -> r
  | r :: rs -> Intersect (r, regex_of_exprset rs)

let rec regex_of_exprsets = function
  | [] -> failwith "empty exprsets" (* Phi? *)
  | [x] -> regex_of_exprset x
  | x :: xs -> Choice (regex_of_exprset x, regex_of_exprsets xs)


let rec pattern_of_exprset = function
  | [] -> failwith "empty exprset" (* Phi? *)
  | [p] -> p
  | p :: ps -> PatIntersect (p, pattern_of_exprset ps)


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


(* circled · *)
let mul_exprsets_expr ess g =
  List.map (List.map (fun e -> Concat (e, g))) ess

let mul_exprsets_expr_pat ?iterate p1ss p2 =
  List.map (fun (p1s, f) ->
    List.map (fun p1 -> PatConcat (p1, p2)) p1s,
    match iterate with
    | None -> f
    | Some iterate -> Tag.compose f iterate
  ) p1ss


(* circled ∩ *)
let intersect_exprsets ess fss =
  (* unite every es with every fs *)
  Util.reduce set_union (
    (* for each E set *)
    List.map (fun es ->
      (* for each F set *)
      List.map (fun fs ->
        (* combine the two sets *)
        set_union es fs
      ) fss
    ) ess
  )


let intersect_exprsets_pat ess fss =
  (* unite every es with every fs *)
  Util.reduce set_union_pat (
    (* for each E set *)
    List.map (fun (es, e_tag) ->
      (* for each F set *)
      List.map (fun (fs, f_tag) ->
        (* combine the two sets and associated
         * transition functions *)
        set_union es fs, Tag.compose e_tag f_tag
      ) fss
    ) ess
  )


(* circled ¬ *)
let not_exprsets sets =
  Util.reduce intersect_exprsets (
    List.map (List.map (fun e -> [Not e])) sets
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
  | Choice (r1, r2) ->
      let r1' = derive l r1 in
      let r2' = derive l r2 in
      set_union r1' r2'
  (* 4 *)
  | Star (r1) as star ->
      mul_exprsets_expr (derive l r1) star
  (* 5 *)
  | Concat (r1, r2) ->
      let r1' = derive l r1 in
      let r1'r2 = mul_exprsets_expr r1' r2 in
      if Language.nullable r1 then
        set_union
          r1'r2
          (derive l r2)
      else
        r1'r2
  (* 6 *)
  | Not r -> not_exprsets (derive l r)
  (* 7 *)
  | Intersect (r1, r2) ->
      intersect_exprsets (derive l r1) (derive l r2)
  (* additional *)
  | Repeat (r, 1) ->
      derive l r
  | Repeat (r, n) ->
      assert (n > 1);
      derive l (Concat (r, Repeat (r, n - 1)))


(* ·\p· :: l -> p -> [p] *)
let rec derive_pat l p =
  List.map (fun (p, t) ->
    List.map Simplify.simplify_pat p, t
  )
  begin match p with
  | VarBase (x, r) ->
      let f = Tag.update x in
      List.map (fun r ->
        [VarBase (x, regex_of_exprset r)], f
      ) (derive l r)
  | VarGroup (x, p) ->
      let update = Tag.update x in
      List.map (fun (p', f) ->
        [VarGroup (x, pattern_of_exprset p')],
        Tag.compose update f
      ) (derive_pat l p)
  | PatChoice (p1, p2) ->
      set_union (derive_pat l p1) (derive_pat l p2)
  | PatConcat (p1, p2) ->
      let p1' = derive_pat l p1 in
      let p1'p2 = mul_exprsets_expr_pat p1' p2 in
      if Language.nullable (Language.regex_of_pattern p1) then
        set_union_pat
          p1'p2
          (derive_pat l p2)
      else
        p1'p2
  | PatIntersect (p1, p2) ->
      intersect_exprsets_pat (derive_pat l p1) (derive_pat l p2)
  | PatStar (p) as star ->
      let iterate = Tag.iterate p in
      mul_exprsets_expr_pat ~iterate (derive_pat l p) star
  | PatRepeat (p, 1) ->
      let iterate = Tag.iterate p in
      List.map (fun (p', f) ->
        p', Tag.compose f iterate
      ) (derive_pat l p)
  | PatRepeat (p, n) ->
      assert (n > 1);
      let iterate = Tag.iterate p in
      let repeat = PatRepeat (p, n - 1) in
      mul_exprsets_expr_pat ~iterate (derive_pat l p) repeat
  | PatNot _ ->
      failwith "Negation in pattern must be resolved before derivation"
  end


let derive_pat l p =
  List.map (fun (p, f) ->
    pattern_of_exprset p, f
  ) (derive_pat l p)
