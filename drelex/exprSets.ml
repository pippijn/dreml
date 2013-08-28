open CorePervasives
open Types


let string_of_exprset es =
  List.map Print.string_of_regex es
  |> String.concat " ∩ "

let string_of_exprsets ess =
  List.map string_of_exprset ess
  |> String.concat " ∪ "


let string_of_exprset_pat es =
  List.map Print.string_of_pattern es
  |> String.concat " ∩ "


let string_of_transition (es, f) =
  string_of_exprset_pat es

let string_of_exprsets_pat ess =
  List.map string_of_transition ess
  |> String.concat " ∪ "


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


let set_union eq a b =
  BatList.unique ~eq (a @ b)


(* circled · *)
let mul_exprsets_expr ess g =
  List.map (List.map (fun e -> Concat (e, g))) ess

let mul_exprsets_expr_pat ?iterate p1ss p2 =
  List.map (fun (p1s, f) ->
    List.map (fun p1 -> PatConcat (p1, p2)) p1s,
    match iterate with
    | None -> f
    | Some iterate -> f % iterate
  ) p1ss


(* circled ∩ *)
let intersect_exprsets ess fss =
  (* unite every es with every fs *)
  Util.reduce (set_union (=)) (
    (* for each E set *)
    List.map (fun es ->
      (* for each F set *)
      List.map (fun fs ->
        (* combine the two sets *)
        set_union (=) es fs
      ) fss
    ) ess
  )


let intersect_exprsets_pat ess fss =
  (* unite every es with every fs *)
  Util.reduce (set_union Util.equal_fst) (
    (* for each E set *)
    List.map (fun (es, e_tag) ->
      (* for each F set *)
      List.map (fun (fs, f_tag) ->
        (* combine the two sets and associated
         * transition functions *)
        set_union (=) es fs, e_tag % f_tag
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
  List.filter (not % List.exists (Simplify.simplify |- Language.is_empty_language))

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
  | Choice (e, f) ->
      set_union (=) (derive l e) (derive l f)
  (* 4 *)
  | Star (e) as star ->
      mul_exprsets_expr (derive l e) star
  (* 5 *)
  | Concat (e, f) ->
      let e_derived = mul_exprsets_expr (derive l e) f in
      if Language.nullable e then
        set_union (=)
          e_derived
          (derive l f)
      else
        e_derived
  (* 6 *)
  | Not e -> not_exprsets (derive l e)
  (* 7 *)
  | Intersect (e, f) ->
      intersect_exprsets (derive l e) (derive l f)
  (* additional *)
  | Repeat (e, 1) ->
      derive l e
  | Repeat (e, n) ->
      assert (n > 1);
      derive l (Concat (e, Repeat (e, n - 1)))


let update x env = env

(* ·\p· :: l -> p -> [p] *)
let rec derive_pat : type a. char -> a pattern -> a exprsets = fun l -> function
  | VarBase (x, r) ->
      let f = update x in
      List.map (fun r ->
        [VarBase (x, regex_of_exprset r)], f
      ) (derive l r)
  | VarGroup (x, p) ->
      let update = update x in
      List.map (fun (p', f) ->
        [VarGroup (x, pattern_of_exprset p')], update % f
      ) (derive_pat l p)
  | PatChoice (p1, p2) ->
      set_union (=) (derive_pat l p1) (derive_pat l p2)
  | PatConcat (p1, p2) ->
      let p1' = derive_pat l p1 in
      let p2' = derive_pat l p2 in
      let p1'p2 = mul_exprsets_expr_pat p1' p2 in
      assert false

let derive_PatConcat : type a b. char -> (a * b) pattern -> (a * b) exprsets * b exprsets = fun l -> function
  | PatConcat (p1, p2) ->
      let p1' = derive_pat l p1 in
      let p2' = derive_pat l p2 in
      let p1'p2 = mul_exprsets_expr_pat p1' p2 in
      
      if Language.nullable (Language.regex_of_pattern p1) then
        (p1'p2, p2')
      else
        (p1'p2, [])


(*
let rec derive_pat : type a. a pattern -> _ = fun p l ->
  match p with
  | PatConcat (p1, p2) ->
      let p1' = derive_pat l p1 in
      let p2' = derive_pat l p2 in
      let p1'p2 = mul_exprsets_expr_pat p1' p2 in
      if Language.nullable (Language.regex_of_pattern p1) then
        set_union (=)
          p1'p2
          p2'
      else
        p1'p2
  | PatIntersect (p1, p2) ->
      intersect_exprsets_pat (derive_pat l p1) (derive_pat l p2)
  | PatStar (e) as star ->
      let iterate = iterate e in
      mul_exprsets_expr_pat ~iterate (derive_pat l e) star
  | PatRepeat (e, 1) ->
      let iterate = iterate e in
      List.map (fun (p', f) ->
        p', f % iterate
      ) (derive_pat l e)
  | PatRepeat (e, n) ->
      assert (n > 1);
      let iterate = iterate e in
      let repeat = PatRepeat (e, n - 1) in
      mul_exprsets_expr_pat ~iterate (derive_pat l e) repeat
  | PatNot _ ->
      failwith "Negation in pattern must be resolved before derivation"
      *)


(*
let derive_pat l p =
  List.map (fun (p, f) ->
    pattern_of_exprset p, f
  ) (derive_pat l p)
*)
