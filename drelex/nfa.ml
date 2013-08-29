open CorePervasives
open Types


(*let _trace = true*)
(*let _trace = false*)
let _trace =
  match Sys.argv with
  | [|_; _; _; "-trace"|] -> true
  | _ -> false


let filter_nonempty states =
  List.filter (
    not
    % Language.is_empty_language
    % Language.regex_of_pattern
    % fst
  ) states


let filter_final states =
  List.filter (
    Language.nullable
    % Language.regex_of_pattern
    % fst
  ) states


let string_of_label varmap label =
  Array.get varmap (label - 1)


let string_of_pattern varmap =
  Print.string_of_pattern (string_of_label varmap)


let transitions varmap p =
  Printf.printf "transitions for %s:\n"
    (string_of_pattern varmap p);
  Array.init 256 (fun n ->
    let chr = Char.chr n in

    let pds =
      ExprSets.derive_pat chr p
      |> filter_nonempty
    in

    if pds != [] then (
      Printf.printf "  on '%s':\n"
        (Char.escaped chr);
      List.iter (fun (pd, f) ->
        Printf.printf "    %-30s\t%s\n"
          (string_of_pattern varmap pd)
          (Tag.to_string (string_of_label varmap) f)
      ) pds;
    );

    pds
  )


let rec build varmap nfa p =
  if not (Hashtbl.mem nfa p) then (
    let xs = transitions varmap p in
    Hashtbl.add nfa p xs;
    Array.iter (List.iter (fun (pd, f) ->
      build varmap nfa pd
    )) xs
  )


let build varmap start =
  let nfa = Hashtbl.create 10 in
  build varmap nfa start;
  nfa, start



let cardinal (nfa, start) =
  Hashtbl.length nfa


let optimised (nfa, start) =
  let nstates = Hashtbl.length nfa in

  (* first, build hashcons *)
  let hashcons = Hashtbl.create nstates in
  Hashtbl.iter (fun p xs ->
    assert (not (Hashtbl.mem hashcons p));
    Hashtbl.add hashcons p (Hashtbl.length hashcons)
  ) nfa;

  (* and the inversion *)
  let inversion = Array.make nstates start in
  Hashtbl.iter (fun p id ->
    inversion.(id) <- p
  ) hashcons;

  (* next, map all patterns to their id *)
  let optimised = Array.make nstates [||] in
  Hashtbl.iter (fun p xs ->
    let p = Hashtbl.find hashcons p in
    let xs = Array.map (BatList.map (fun (pd, f) ->
      Hashtbl.find hashcons pd, f
    )) xs in
    optimised.(p) <- xs
  ) nfa;

  let start = Hashtbl.find hashcons start in

  let inversion (state, env) =
    inversion.(state), env
  in

  optimised, start, inversion


let string_of_label varmap x =
  if x < 0 then
    varmap.(-x - 1) ^ "'"
  else
    varmap.( x - 1)


let show ?(pre="") varmap input states =
  List.iter (fun (p, env) ->
    let is_final =
      if Language.nullable (Language.regex_of_pattern p) then
        "\t(FINAL)"
      else
        ""
    in
    print_endline (
      "  state: " ^
      pre ^
      string_of_pattern varmap p ^
      is_final
    );
    print_endline (
      "  env:   [" ^
      String.concat ", " (
        List.map (fun (x, (Pos (start_p, end_p))) ->
          Printf.sprintf "(%s: \"%s\")"
            (string_of_label varmap x)
            (String.sub input start_p (end_p - start_p + 1))
        ) env
      ) ^
      "]"
    );
  ) states


let show_internal inversion varmap input states =
  List.iter (fun (p, env) ->
    let (p', env) = inversion (p, env) in
    show ~pre:(string_of_int p ^ ": ") varmap input [p', env]
  ) states


let run (nfa, start) varmap input env =
  let (nfa, start, inversion) = optimised (nfa, start) in
  let seen = Array.create (Array.length nfa) false in

  BatString.fold_left (fun (pos, states) c ->
    if _trace then print_newline ();

    for i = 0 to Array.length seen - 1 do
      seen.(i) <- false
    done;

    let states =
      BatList.fold_left (fun states (state, env) ->
        (* get the transition tables for the current states *)
        let table = nfa.(state) in
        (* find all transitions on 'c' *)
        let next = table.(Char.code c) in

        if _trace then (
          Printf.printf "state %d -> [%s]\n"
            state (String.concat ";" (List.map (string_of_int % fst) next))
        );

        (* update envs *)
        BatList.fold_left (fun states (pd, f) ->
          if seen.(pd) then (
            states
          ) else (
            seen.(pd) <- true;
            states @ [(pd, Tag.execute f pos env)]
          )
        ) states next
      ) [] states
    in

    if _trace then (
      Printf.printf "after %s: in %d states\n" (Char.escaped c) (List.length states);
      show_internal inversion varmap input states;
    );

    pos + 1, states
  ) (0, [start, env]) input
  |> snd
  |> BatList.map inversion
