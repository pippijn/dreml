open Types


(* 4.1 Weaker notions of RE equivalence *)
let rec simplify = function
  (* & *)
  | Intersect (_, r1, r2) when r1 = r2 -> r1

  | Intersect (_, Phi, _)
  | Intersect (_, _, Phi) -> Phi

  | Intersect (_, Not (_, Phi), r)
  | Intersect (_, r, Not (_, Phi)) -> r

  (* juxtaposition *)
  | Concat (_, Phi, _)
  | Concat (_, _, Phi) -> Phi

  | Concat (_, Epsilon, r)
  | Concat (_, r, Epsilon) -> r

  (* + *)
  | Choice (_, r1, r2) when r1 = r2 -> r1

  | Choice (_, Not (null, Phi), _)
  | Choice (_, _, Not (null, Phi)) -> Not (null, Phi)

  | Choice (_, Phi, r)
  | Choice (_, r, Phi) -> r

  (* * *)
  | Star (Star _ as inner) -> inner
  | Star Epsilon -> Epsilon
  | Star Phi -> Epsilon

  (* ~(~r) -> r *)
  | Not (_, Not (_, r)) -> r

  (* ~eps -> phi *)
  | Not (_, Epsilon) -> Phi


  (* XXX: this simplification makes this testcase trivial and useless:
     (a: ((a+b)*a(a+b)^3) & ((~((~a)&(~b)))*a(a+b)^3) )
  *)

  (* remove negation by De Morgan, if possible *)
  (*| Not (Intersect (Not r1, Not r2)) -> Choice (r1, r2)*)
  (*| Not (Choice (Not r1, Not r2)) -> Intersect (r1, r2)*)

  | Repeat (_, _, 0) -> Epsilon
  | Repeat (_, r, 1) -> r

  (* Recursive simplify *)
  | Concat (null, r1, r2) -> Concat (null, simplify r1, simplify r2)
  | Choice (null, r1, r2) -> Choice (null, simplify r1, simplify r2)
  | Intersect (null, r1, r2) -> Intersect (null, simplify r1, simplify r2)
  | Star r -> Star (simplify r)
  | Repeat (null, r, n) -> Repeat (null, simplify r, n)
  | Not (null, r) -> Not (null, simplify r)

  (* Keep atoms *)
  | Phi
  | Epsilon
  | Letter _ as r -> r

let simplify = Util.rewrite simplify
let simplify x = x

  
let rec simplify_pat = function
  (* We need to catch this here to ensure termination *)
  | PatConcat (_, VarBase (_, _, Epsilon), r)
  | PatConcat (_, r, VarBase (_, _, Epsilon)) -> r

  (* * *)
  | PatStar (PatStar _ as inner) -> inner

  (* Recursive simplify *)
  | VarBase (null, x, r) -> VarBase (null, x, simplify r)
  | VarGroup (null, x, p) -> VarGroup (null, x, simplify_pat p)

  | PatIntersect (null, p1, p2) -> PatIntersect (null, simplify_pat p1, simplify_pat p2)
  | PatConcat (null, p1, p2) -> PatConcat (null, simplify_pat p1, simplify_pat p2)
  | PatChoice (null, p1, p2) -> PatChoice (null, simplify_pat p1, simplify_pat p2)
  | PatStar p -> PatStar (simplify_pat p)
  | PatRepeat (null, p, n) -> PatRepeat (null, simplify_pat p, n)
  | PatNot (null, p) -> PatNot (null, simplify_pat p)

let simplify_pat x = Util.rewrite simplify_pat x
let simplify_pat x = x
