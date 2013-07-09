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

  BatList.unique (
    List.map (fun (state, env) ->
      state, BatList.unique env
    ) states
  )
