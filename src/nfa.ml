open Types

module type DeriveType = sig

  module Make : functor(Lbl : LabelType) -> functor(Tag : TransitionType) -> sig
    val derive_pat : letter -> Lbl.t pattern -> (Lbl.t pattern * Lbl.t Tag.transition) list
  end

end

module Make(Derive : DeriveType)(Lbl : LabelType)(Tag : TransitionType) = struct

  open CorePervasives

  module Derive = Derive.Make(Lbl)(Tag)
  module Tag = Tag.Make(Lbl)


  type partial_derivative = (Lbl.t pattern * Tag.t) list
  type transition = partial_derivative array
  type nfa = (Lbl.t pattern, transition) Hashtbl.t

  let _trace = true


  let transitions derive p =
    if _trace then
      Printf.printf "transitions for %s:\n"
        (Util.string_of_pattern Lbl.to_string p);
    Array.init 256 (fun n ->
      let chr = Char.chr n in

      let pds =
        derive chr p
        |> Dreml.pat_funs_simplify_list
      in

      if _trace && pds != [] then (
        Printf.printf "  on '%s':\n"
          (Char.escaped chr);
        List.iter (fun (pd, f) ->
          print_endline (
            "    " ^
            Util.string_of_pattern Lbl.to_string (Simplify.simplify_pat pd) ^
            "\t" ^
            Tag.to_string f
          )
        ) pds;
      );

      pds
    )


  let rec build nfa p =
    if not (Hashtbl.mem nfa p) then (
      let xs = transitions Derive.derive_pat p in
      Hashtbl.add nfa p xs;
      Array.iter (List.iter (fun (pd, f) ->
        build nfa pd
      )) xs
    )


  let build start =
    let nfa = Hashtbl.create 10 in
    build nfa start;
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

    optimised, start, inversion


  let filter_final states =
    List.filter (
      Language.nullable % Language.regex_of_pattern % fst
    ) states


  let show states =
    List.iter (fun (p, env) ->
      let is_final =
        if Language.nullable (Language.regex_of_pattern p) then
          "\t(FINAL)"
        else
          ""
      in
      print_endline ("  state: " ^ Util.string_of_pattern Lbl.to_string p ^ is_final);
      print_endline ("  env:   " ^ Show.show<Lbl.t env> env);
    ) states


  let run (nfa, start) input env =
    BatString.fold_left (fun (pos, states) c ->
      pos + 1, BatList.map (fun (state, env) ->
        (* get the transition tables for the current states *)
        let table = Hashtbl.find nfa state in
        (* find all transitions on 'c' *)
        table.(Char.code c), env
      ) states
      (* update envs *)
      |> BatList.map (fun (next, env) ->
           BatList.map (fun (pd, f) ->
             pd, Tag.execute pos f env
           ) next
         )
      (* flatten new state list, as each state may have gone to several other states *)
      |> BatList.flatten
      |> Duplicates.remove_duplicate_results Lbl.unrename true
    ) (0, [start, env]) input
    |> snd


  let run_optimised (nfa, start) input env =
    let (nfa, start, inversion) = optimised (nfa, start) in

    Timing.time "run" (BatString.fold_left (fun (pos, states) c ->
      let states =
        BatList.map (fun (state, env) ->
          (* get the transition tables for the current states *)
          let table = nfa.(state) in
          (* find all transitions on 'c' *)
          table.(Char.code c), env
        ) states
      in

      (* update envs *)
      let states =
        List.fold_left (fun flat (next, env) ->
          List.fold_left (fun flat (pd, f) ->
            (pd, Tag.execute pos f env) :: flat
          ) flat next
        ) [] states
      in

      let states =
        Duplicates.remove_duplicate_results Lbl.unrename true states
      in

      pos + 1, states
    ) (0, [start, env])) input
    |> snd

    |> BatList.map (fun (state, env) ->
         inversion.(state), env
       )

end
