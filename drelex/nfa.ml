open CorePervasives
open Types

let _trace_con = false
let _trace_run = false

module Debug = struct

  let time label f =
    let s = Unix.gettimeofday () in
    let r = f () in
    let e = Unix.gettimeofday () in
    Printf.printf "%s: %.06f sec\n" label (e -. s);
    flush stdout;
    r


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

module BitSet = struct

  type bitset = string

  let bitset_create n =
    let size = n / 8 + (if n mod 8 = 0 then 0 else 1) in
    String.make size '\000'

  let bitset_mem seen x =
    let pos = x / 8 in
    let off = x mod 8 in
    let c = Char.code (String.unsafe_get seen pos) in
    (c land (1 lsl off)) <> 0

  let bitset_set seen x =
    let pos = x / 8 in
    let off = x mod 8 in
    let c = Char.code (String.unsafe_get seen pos) in
    let mask = 1 lsl off in
    if (c land mask) = 0 then
      String.unsafe_set seen pos (Char.unsafe_chr (c lor mask))


  let bitset_unset seen x =
    let pos = x / 8 in
    let off = x mod 8 in
    let c = Char.code (String.unsafe_get seen pos) in
    let mask = 1 lsl off in
    if (c land mask) <> 0 then
      String.unsafe_set seen pos (Char.unsafe_chr (c lxor mask))


  let bitset_clear seen =
    String.fill seen 0 (String.length seen) '\000'

  let bitset_print seen =
    for i = 0 to String.length seen * 8 do
      print_char (
        if bitset_mem seen i then
          '1'
        else
          '0'
      )
    done;
    print_newline ()

end

type bitset = bool array

let bitset_create n =
  Array.create n false

let bitset_mem seen x =
  Array.unsafe_get seen x

let bitset_set seen x =
  Array.unsafe_set seen x true

let bitset_unset seen x =
  Array.unsafe_set seen x false


type 'tag nfa = {
  seen : bitset;
  nfa : (int * 'tag) list array;
  input : string;
  len : int;
}

let update_envs seen pos env states next =
  let rec update_envs0 seen pos env states = function
    | (pd, f) :: tl ->
        let states =
          if bitset_mem seen pd then (
            states
          ) else (
            bitset_set seen pd;
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

        if _trace_run then (
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


let rec clear_seen seen = function
  | [] -> ()
  | (number, _) :: states ->
      bitset_unset seen number;
      clear_seen seen states


let iteration nfa pos states c =
  let next = goto_next_states nfa pos c states in
  clear_seen nfa.seen next;
  next


let rec main_loop inversion varmap nfa pos states =
  if _trace_run then
    print_newline ();

  if pos = nfa.len then
    states
  else
    let c = String.unsafe_get nfa.input pos in
    let states = iteration nfa pos states c in

    if _trace_run then (
      Printf.printf "after %s: in %d states\n" (Char.escaped c) (List.length states);
      Debug.show_internal inversion varmap nfa.input states;
    );

    main_loop inversion varmap nfa (pos + 1) states


let run inversion varmap nfa start input =
  let seen = bitset_create (Array.length nfa / 256) in
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
  if _trace_con then (
    Printf.printf "transitions for %s:\n"
      (Debug.string_of_pattern varmap p);
  );
  Array.init 256 (fun n ->
    let chr = Char.chr n in

    let pds =
      ExprSets.derive_pat chr p
      |> filter_nonempty
    in

    if _trace_con && pds != [] then (
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
  Debug.time "build" (fun () ->
    let nfa = Hashtbl.create 10 in
    build varmap nfa start;
    nfa, start
  )



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


let run (nfa, start) varmap input =
  Gc.(set { (Gc.get ()) with
    minor_heap_size = 4096 * 8;
  });

  let (nfa, start, inversion) =
    Debug.time "hashcons" (fun () ->
      optimised (nfa, start)
    )
  in

  Debug.time "run" (fun () ->
    run inversion varmap nfa start input
    |> BatList.map inversion
  )
