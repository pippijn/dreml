Require Import Regex.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.
Require Import Coq.Lists.ListSet.
Require Language.


Definition set_eq {A : Type}
                  (eq : forall x y : A, {x = y} + {x <> y})
                : forall x y : set A, {x = y} + {x <> y}.
  decide equality; apply eq.
Defined.


Fixpoint set_flatten
  {A : Type}
  (eq : forall x y : A, {x = y} + {x <> y})
  (l : set (set A)) : set A :=
  match l with
  | nil => nil
  | cons hd tl => set_union eq hd (set_flatten eq tl)
  end.


Definition mul_exprsets_expr (ess : exprsets) (g : regex) : exprsets :=
  map (map (fun e => Concat e g)) ess.

Definition mul_exprsets_expr_pat (iterate : env -> env)
                                 (ess : exprsets_pat)
                                 (g : pattern)
                                 : exprsets_pat :=
  map (fun pair =>
    let '(es, tag) := pair in
    (map (fun e => PatConcat e g) es, compose tag iterate)
  ) ess.


Definition intersect_exprsets (ess fss : exprsets) : exprsets :=
  set_flatten (set_eq regex_eq) (
    set_map (set_eq (set_eq regex_eq)) (fun es =>
      set_map (set_eq regex_eq) (fun fs =>
        set_union regex_eq es fs
      ) fss
    ) ess
  ).

Definition intersect_exprsets_pat (ess fss : exprsets_pat) : exprsets_pat :=
  set_flatten (set_eq pattern_eq) (
    map (fun pair =>
      let '(es, e_tag) := pair in
      map (fun pair =>
        let '(fs, f_tag) := pair in
        (set_union pattern_eq es fs, compose e_tag f_tag)
      ) fss
    ) ess
  ).


Definition not_exprsets (sets : exprsets) : exprsets :=
  match map (map (fun e => cons (Not e) nil)) sets with
  | nil => nil
  | cons hd tl => fold_left intersect_exprsets tl hd
  end.


Fixpoint derive (l : ascii) (re : regex) : exprsets :=
  match re with
  | Letter l2 =>
      if ascii_dec l l2 then
        cons (cons Epsilon nil) nil
      else
        cons (cons Phi nil) nil

  | Phi
  | Epsilon =>
      cons (cons Phi nil) nil

  | Choice e f =>
      set_union (set_eq regex_eq) (derive l e) (derive l f)

  | Star e =>
      mul_exprsets_expr (derive l e) re

  | Concat e f =>
      let e_derived := mul_exprsets_expr (derive l e) f in
      if Language.nullable e then
        set_union (set_eq regex_eq)
          e_derived
          (derive l f)
      else
        e_derived

  | Not e =>
      not_exprsets (derive l e)

  | Intersect e f =>
      intersect_exprsets (derive l e) (derive l f)
  end.
