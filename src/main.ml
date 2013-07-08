open CorePervasives


(*module Nfa = Nfa.Make(Partial)(Transition)*)
(*module Nfa = Nfa.Make(ExprSets)(Transition)*)
(*module Nfa = Nfa.Make(Partial)(Instruction)*)
module Nfa = Nfa.Make(ExprSets)(Instruction)


let _visualise = false
let _show_result = true


let check_clashes re =
  match Pattern.variable_clashes re with
  | [] -> ()
  | vars -> failwith ("variable clashes: " ^ Show.show<string list> vars)


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
      Visualise.graphviz_of_nfa out nfa
    ) (open_out "graph.dot");
    ignore (Sys.command "dot graph.dot -Tpng -o graph.png");
  );
  print_endline ("start state: " ^ Util.string_of_pattern re);
  print_endline ("number of states: " ^ string_of_int (Nfa.cardinal nfa));
  let final = Timing.time "run nfa" (Nfa.run nfa input) Types.empty_env in
  let unique = Duplicates.remove_duplicate_results false final in
  if _show_result then (
    print_endline ("finished after string: \"" ^ input ^ "\"");
    Nfa.show final;
    print_endline "unique:";
    Nfa.show unique;
  );

  Printf.printf "%d vs %d\n" (List.length final) (List.length unique);
;;


let () =
  Printexc.record_backtrace true;

  match Sys.argv with
  | [|_; pattern; input|] ->
      main pattern input
  | [|_; pattern|] ->
      main pattern ""
  | _ ->
      main "(a:(a(b|c)d)*)(b:(ac)*)" "abdac"
