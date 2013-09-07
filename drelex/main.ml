open CorePervasives
open Types


let a =
  VarBase ("x1",
    Star (Letter 'a')
  )

let b =
  VarBase ("x2",
    Concat (Star (Letter 'a'), Letter 'b')
  )

let lexer =
  VarGroup ("x",
    PatChoice (a, b)
  )


let input = String.make (10 * 1024 * 1024) 'a' ^ "b"


let a () =
  let lexer, varmap = Pattern.number_pattern lexer in

  let (nfa, start) = Nfa.build varmap lexer in
  let states = Nfa.run (nfa, start) varmap input in

  if false then
    Nfa.show varmap input states
;;


let b pattern input =
  let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
  let lexer, varmap = Pattern.number_pattern lexer in

  print_endline "br1";
  let (nfa, start) = Nfa.build varmap lexer in
  print_endline "br2";
  let states = Nfa.run (nfa, start) varmap input in

  if false then (
    print_endline ("after pattern: " ^ pattern);
    Nfa.show varmap input states
  )


let () =
  Printexc.record_backtrace true;

  match Sys.argv with
  | [|_; pattern; input; _|]
  | [|_; pattern; input|] ->
      b pattern input

  | [|_; pattern|] ->
      b pattern input

  | _ ->
      a ()
