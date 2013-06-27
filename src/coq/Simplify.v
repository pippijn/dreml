Require Import Regex.

Fixpoint simplify r :=
  match r with
  (* & *)
  | Intersect _ Phi
  | Intersect Phi _ => Phi

  | Intersect (Not Phi) r
  | Intersect r (Not Phi) => r

  | Intersect r1 r2 =>
      if regex_eq r1 r2 then
        r1
      else
        Intersect (simplify r1) (simplify r2)

  (* juxtaposition *)
  | Concat Phi _
  | Concat _ Phi => Phi

  | Concat Epsilon r
  | Concat r Epsilon => r

  (* + *)
  | Choice (Not Phi) _
  | Choice _ (Not Phi) => Not Phi

  | Choice Phi r
  | Choice r Phi => r

  | Choice r1 r2 =>
      if regex_eq r1 r2 then
        r1
      else
        Choice (simplify r1) (simplify r2)

  (* * *)
  | Star (Star r) => Star r

  | Star Epsilon => Epsilon
  | Star Phi => Epsilon

  (* ~(~r) = r *)
  | Not (Not r) => r

  (* ~eps = phi *)
  | Not Epsilon => Phi

  (* remove negation by De Morgan, if possible *)
  | Not (Intersect (Not r1) (Not r2)) => Choice r1 r2
  | Not (Choice (Not r1) (Not r2)) => Intersect r1 r2

  (* Recursive simplify *)
  | Concat r1 r2 => Concat (simplify r1) (simplify r2)
  | Star r => Star (simplify r)
  | Not r => Not (simplify r)

  (* keep atoms *)
  | Phi => Phi
  | Epsilon => Epsilon
  | Letter l => Letter l
  end.


Fixpoint simplify_pat r :=
  match r with
  (* We need to catch this here to ensure termination *)
  | PatConcat (VarBase _ Epsilon) r
  | PatConcat r (VarBase _ Epsilon) => r

  (* Recursive simplify *)
  | VarBase x r => VarBase x (simplify r)
  | VarGroup x p => VarGroup x (simplify_pat p)

  | PatIntersect r1 r2 => PatIntersect (simplify_pat r1) (simplify_pat r2)
  | PatConcat r1 r2 => PatConcat (simplify_pat r1) (simplify_pat r2)
  | PatChoice r1 r2 => PatChoice (simplify_pat r1) (simplify_pat r2)
  | PatStar r => PatStar (simplify_pat r)
  end.
