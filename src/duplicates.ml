let rec unique_cmp uniq seen cmp l =
  match l with
  | x :: xs ->
      if BatMap.mem x seen then (
        unique_cmp uniq seen cmp xs
      ) else (
        let seen = BatMap.add x () seen in
        unique_cmp (x :: uniq) seen cmp xs
      )
  | [] ->
      List.rev uniq
      


let unique_cmp ?(cmp=compare) l =
  let seen = BatMap.create cmp in
  unique_cmp [] seen cmp l


(* Keep all matches. *)
let remove_duplicate_results in_nfa states =
  let states =
    if in_nfa then
      states
    else
      List.map (fun (state, env) ->
        state, List.map (fun (x, w) ->
          if x.[0] = '\'' then
            (String.sub x 1 (String.length x - 1), w)
          else
            (x, w)
        ) env
      ) states
  in

  unique_cmp (
    List.map (fun (state, env) ->
      state, unique_cmp env
    ) states
  )


(* Greedy left-most match. *)
let remove_duplicate_results in_nfa states =
  let states =
    if in_nfa then
      states
    else
      List.map (fun (state, env) ->
        state, List.map (fun (x, w) ->
          if x.[0] = '\'' then
            (String.sub x 1 (String.length x - 1), w)
          else
            (x, w)
        ) env
      ) states
  in

  unique_cmp ~cmp:(fun (state1, _) (state2, _) -> compare state1 state2) states
