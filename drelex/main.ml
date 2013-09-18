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
    Nfa.Debug.show varmap input states
;;


let b pattern input =
  let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
  let lexer, varmap = Pattern.number_pattern lexer in

  let (nfa, start) = Nfa.build varmap lexer in
  let states = Nfa.run (nfa, start) varmap input in

  if true then (
    print_endline ("after pattern: " ^ pattern);
    Nfa.Debug.show varmap input states
  )


let main () =
  Printexc.record_backtrace true;

  match Sys.argv with
  | [|_; pattern; input; _|]
  | [|_; pattern; input|] ->
      b pattern input

  | [|_; pattern|] ->
      b pattern input

  | _ ->
      a ()


let timing1 () =
  Gc.(set { (Gc.get ()) with
    minor_heap_size = 4096 * 8;
  });

  let fh = open_out "results.log" in

  for i = 1 to 30 do
    Gc.compact ();

    let pattern = ref "" in
    for n = 1 to i do
      pattern := !pattern ^ Printf.sprintf "(x%d:a)" n;
    done;

    let lexer = Parser.start Lexer.token (Lexing.from_string !pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let min_time = ref 1000.0 in

    for t = 1 to 20 do
      let s = Unix.gettimeofday () in
      ignore (Nfa.build varmap lexer);
      let e = Unix.gettimeofday () in

      if !min_time > (e -. s) then (
        min_time := e -. s;
        Printf.printf "%d,%06f\r" i !min_time;
        flush stdout;
      );
    done;

    Printf.fprintf fh "%d,%06f\n" i !min_time;
  done


(*let () = main ()*)
let () = timing1 ()
