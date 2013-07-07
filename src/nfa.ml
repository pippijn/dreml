module type DeriveType = sig

  module Make : functor(Tag : Types.TransitionType) -> sig
    val derive_pat : Types.letter -> Types.pattern -> (Types.pattern * Tag.t) list
  end

end

module Make(Derive : DeriveType)(Tag : Types.TransitionType) = struct

  open CorePervasives
  open Types

  module Derive = Derive.Make(Tag)


  let _trace = false


  let transitions derive p =
    if _trace then
      Printf.printf "transitions for %s:\n"
        (Util.string_of_pattern p);
    Array.init 256 (fun n ->
      if n < Char.code 'a' || n > Char.code 'd' then [] else

      let chr = Char.chr n in

      let pds =
        derive chr p
        |> Dreml.pat_funs_simplify_list
      in

      if _trace && pds != [] then (
        Printf.printf "  on '%s':\n"
          (Char.escaped chr);
        List.iter (fun (pd, f) ->
          print_endline ("    " ^ Util.string_of_pattern pd)
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
      let xs = Array.map (List.map (fun (pd, f) ->
        Hashtbl.find hashcons pd, f
      )) xs in
      optimised.(p) <- xs
    ) nfa;

    let start = Hashtbl.find hashcons start in

    optimised, start, inversion


  let show states =
    List.iter (fun (p, env) ->
      let is_final =
        if Language.nullable (Language.regex_of_pattern p) then
          "\t(FINAL)"
        else
          ""
      in
      print_endline ("  state: " ^ Util.string_of_pattern p ^ is_final);
      print_endline ("  env:   " ^ Show.show<env> env);
    ) states


  let reverse_env states =
    if Transition._reverse then
      List.map (fun (state, env) ->
        state, List.map (fun (x, w) -> x, List.rev w) env
      ) states
    else
      states


  let run (nfa, start) input env =
    BatString.fold_left (fun states c ->
      List.rev_map (fun (state, env) ->
        (* get the transition tables for the current states *)
        let table = Hashtbl.find nfa state in
        (* find all transitions on 'c' *)
        table.(Char.code c), env
      ) states
      (* update envs *)
      |> List.rev_map (fun (next, env) ->
           List.rev_map (fun (pd, f) ->
             pd, Tag.execute f env
           ) next
         )
      (* flatten new state list, as each state may have gone to several other states *)
      |> List.flatten
      (*|> Duplicates.remove_duplicate_results true*)
    ) [start, env] input

    |> reverse_env


  let run_optimised (nfa, start) input env =
    let (nfa, start, inversion) = optimised (nfa, start) in

    Timing.time "run" (BatString.fold_left (fun states c ->
      List.rev_map (fun (state, env) ->
        (* get the transition tables for the current states *)
        let table = nfa.(state) in
        (* find all transitions on 'c' *)
        table.(Char.code c), env
      ) states
      (* update envs *)
      |> List.rev_map (fun (next, env) ->
           List.rev_map (fun (pd, f) ->
             pd, Tag.execute f env
           ) next
         )
      (* flatten new state list, as each state may have gone to several other states *)
      |> List.flatten
      |> Duplicates.remove_duplicate_results true
    ) [start, env]) input

    |> reverse_env
    |> List.rev_map (fun (state, env) ->
         inversion.(state), env
       )

end
