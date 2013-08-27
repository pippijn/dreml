open CorePervasives


module Make(Derive : Nfa.DeriveType)(Lbl : Types.LabelType)(Tag : Types.TransitionType) = struct

  module Nfa = Nfa.Make(Derive)(Lbl)(Tag)

  module Parser = Parser.Make(Lbl)
  module Lexer = Lexer.Make(Lbl)

  let _visualise = false
  let _show_result = true


  let check_clashes re =
    match Pattern.variable_clashes re with
    | [] -> ()
    | vars -> failwith ("variable clashes: " ^ Show.show<Lbl.t list> vars)


  let main pattern input =
    let open Types in

    let re =
      let lexbuf = Lexing.from_string pattern in
      Parser.start Lexer.token lexbuf
    in

    check_clashes re;
    let re = Negation.normal_form re in

    let nfa = Timing.time "build nfa" Nfa.build re in
    if _visualise then (
      BatPervasives.with_dispose ~dispose:close_out (fun out ->
        Visualise.graphviz_of_nfa Lbl.to_string out nfa
      ) (open_out "graph.dot");
      ignore (Sys.command "dot graph.dot -Tpng -o graph.png");
    );
    print_endline ("start state: " ^ Util.string_of_pattern Lbl.to_string re);
    print_endline ("number of states: " ^ string_of_int (Nfa.cardinal nfa));
    let states = Timing.time "run nfa" (Nfa.run nfa input) Types.empty_env in
    let unique = Duplicates.remove_duplicate_results Lbl.unrename false states in
    let final  = Nfa.filter_final unique in
    if _show_result then (
      print_endline ("finished after string: \"" ^ input ^ "\"");
      Nfa.show states;
      print_endline "unique:";
      Nfa.show unique;
      print_endline "final:";
      Nfa.show final;
    );

    Printf.printf "%d states, %d unique, %d final\n"
      (List.length states)
      (List.length unique)
      (List.length final)
      ;
  ;;

  let bench () =
    let re =
      let lexbuf = Lexing.from_string "(x:a*)*" in
      Parser.start Lexer.token lexbuf
    in

    let input = String.make (1024 * 1024) 'a' in

    check_clashes re;
    let re = Negation.normal_form re in

    let nfa = Timing.time "build nfa" Nfa.build re in
    let final = Timing.time "run nfa" (Nfa.run_optimised nfa input) Types.empty_env in

    ()
  ;;


  let () =
    Printexc.record_backtrace true;

    Cmdline.run ignore;

    match Sys.argv with
    | [|_; "-timing"|] ->
        bench ()

    | [|_; "-timing"; pattern; input|] ->
        main pattern input
    | [|_; "-timing"; pattern|] ->
        main pattern ""

    | [|_; pattern; input|] ->
        main pattern input
    | [|_; pattern|] ->
        main pattern ""

    | _ ->
        main "(a:(a(b|c)d)*)(b:(ac)*)" "abdac"

end


(*module Nfa = Nfa.Make(Partial)(Transition)*)
(*module Nfa = Nfa.Make(ExprSets)(Transition)*)
(*module Nfa = Nfa.Make(Partial)(Instruction)*)
(*module Main = Make(ExprSets)(Labels.String)(Instruction)*)
module Main = Make(ExprSets)(Labels.Int)(Instruction)
