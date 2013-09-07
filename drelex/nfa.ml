open CorePervasives
open Types

let _trace = false

module Debug = struct

  let string_of_label varmap label =
    Array.get varmap (label - 1)


  let string_of_pattern varmap =
    Print.string_of_pattern (string_of_label varmap)


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

end

(**********************************************************
 * :: NFA Simulation.
 **********************************************************)

type 'tag nfa = {
  seen : bool array;
  nfa : (int * 'tag) list array;
  input : string;
  len : int;
}

let update_envs seen pos env states next =
  let rec update_envs0 seen pos env states = function
    | (pd, f) :: tl ->
        let states =
          if Array.unsafe_get seen (pd) then (
            states
          ) else (
            Array.unsafe_set seen (pd) true;
            (* This is slow if there are many states. *)
            states @ [(pd, Tag.execute f pos env)]
          )
        in
        update_envs0 seen pos env states tl

    | [] ->
        states
  in
  update_envs0 seen pos env states next


let goto_next_states nfa pos c curr_states =
  let rec goto_next_states0 nfa pos c next_states curr_states =
    match curr_states with
    | (state, env) :: tl ->
        (* find all transitions on 'c' for 'state' *)
        let next = Array.unsafe_get nfa.nfa (state * 256 + (Char.code c)) in

        if _trace then (
          Printf.printf "state %d -> [%s]\n"
            state (String.concat ";" (List.map (string_of_int % fst) next))
        );

        (* update envs *)
        let next_states = update_envs nfa.seen pos env next_states next in

        (* recursive call *)
        goto_next_states0 nfa pos c next_states tl

    | [] ->
        next_states
  in
  goto_next_states0 nfa pos c [] curr_states


let clear_seen seen =
  for i = 0 to Array.length seen - 1 do
    Array.unsafe_set seen (i) false
  done


let iteration nfa pos states c =
  clear_seen nfa.seen;
  goto_next_states nfa pos c states


let rec main_loop inversion varmap nfa pos states =
  if _trace then
    print_newline ();

  if pos = nfa.len then
    states
  else
    let c = String.unsafe_get nfa.input pos in
    let states = iteration nfa pos states c in

    if _trace then (
      Printf.printf "after %s: in %d states\n" (Char.escaped c) (List.length states);
      Debug.show_internal inversion varmap nfa.input states;
    );

    main_loop inversion varmap nfa (pos + 1) states


let run inversion varmap nfa start input =
  let seen = Array.create (Array.length nfa / 256) false in
  let nfa = { nfa; seen; input; len = String.length input; } in

  main_loop inversion varmap nfa 0 [start, empty_env]



(**********************************************************
 * :: NFA Construction.
 **********************************************************)

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


let transitions varmap p =
  Printf.printf "transitions for %s:\n"
    (Debug.string_of_pattern varmap p);
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
          (Debug.string_of_pattern varmap pd)
          (Tag.to_string (Debug.string_of_label varmap) f)
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
  let optimised = Array.make (nstates * 256) [] in
  Hashtbl.iter (fun p xs ->
    let p = Hashtbl.find hashcons p in
    Array.iteri (fun i x ->
      optimised.(p * 256 + i) <-
        BatList.map (fun (pd, f) ->
          Hashtbl.find hashcons pd, f
        ) x;
    ) xs;
  ) nfa;

  let start = Hashtbl.find hashcons start in

  let inversion (state, env) =
    inversion.(state), env
  in

  optimised, start, inversion


let time f =
  let s = Unix.gettimeofday () in
  let r = f () in
  let e = Unix.gettimeofday () in
  Printf.printf "time: %.06f sec\n" (e -. s);
  r


let run (nfa, start) varmap input =
  Gc.(set { (Gc.get ()) with
    minor_heap_size = 4096 * 8;
  });

  time (fun () ->
    let (nfa, start, inversion) = optimised (nfa, start) in
    run inversion varmap nfa start input
    |> BatList.map inversion
  )
