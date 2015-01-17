open CorePervasives
open Types


let a =
  VarBase (Maybe, "x1",
    Star (Letter 'a')
  )

let b =
  VarBase (Maybe, "x2",
    Concat (Maybe, Star (Letter 'a'), Letter 'b')
  )

let lexer =
  VarGroup (Maybe, "x",
    PatChoice (Maybe, a, b)
  )


let input = String.make (10 * 1024 * 1024) 'a' ^ "b"


let a () =
  let lexer, varmap = Pattern.number_pattern lexer in
  let lexer = Language.compute_nullable_pat lexer in

  let (nfa, start) = Nfa.build varmap lexer in
  let states = Nfa.run (nfa, start) varmap input in

  if false then
    Nfa.Debug.show varmap input states
;;


let b pattern input =
  let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
  let lexer, varmap = Pattern.number_pattern lexer in
  let lexer = Language.compute_nullable_pat lexer in

  let (nfa, start) = Nfa.build varmap lexer in
  Printf.printf "%d states\n" (Hashtbl.length nfa);
  flush stdout;
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

  let fh = open_out "a.log" in

  for i = 1 to 50 do
    let pattern = ref "(x:" in
    for n = 1 to i do
      pattern := !pattern ^ Printf.sprintf "a";
    done;
    let pattern = !pattern ^ ")" in

    let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let lexer = Language.compute_nullable_pat lexer in

    let min_time = ref 1000.0 in

    for t = 1 to 20 do
      Gc.compact ();
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
  done;

  close_out fh


let timing2 () =
  Gc.(set { (Gc.get ()) with
    minor_heap_size = 4096 * 8;
  });

  let fh = open_out "con-an.log" in

  for i = 1 to 30 do
    let pattern = "(x:a^" ^ string_of_int i ^ ")" in

    let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let lexer = Language.compute_nullable_pat lexer in

    let min_time = ref 1000.0 in

    for t = 1 to 200 do
      Gc.compact ();
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
  done;

  close_out fh


let timing3 () =
  Gc.(set { (Gc.get ()) with
    minor_heap_size = 4096 * 8;
  });

  let fh = open_out "results.log" in

  for i = 1 to 50 do
    let pattern = ref "" in
    for n = 1 to i do
      pattern := !pattern ^ Printf.sprintf "(x%d:a)" n;
    done;
    let pattern = !pattern in

    let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let lexer = Language.compute_nullable_pat lexer in

    let min_time = ref 1000.0 in

    for t = 1 to 20 do
      Gc.compact ();
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
  done;

  close_out fh


let timing4 () =
  let lexer = Parser.start Lexer.token (Lexing.from_string "(x:a*)") in
  let lexer, varmap = Pattern.number_pattern lexer in

  let lexer = Language.compute_nullable_pat lexer in

  let (nfa, start) = Nfa.build varmap lexer in

  let fh = open_out "astar.log" in

  for i = 1 to 50 do
    let input = String.make (i * 1024) 'a' in

    let min_time = ref 1000.0 in

    for t = 1 to 100 do
      Gc.compact ();
      let s = Unix.gettimeofday () in
      ignore (Nfa.run (nfa, start) varmap input);
      let e = Unix.gettimeofday () in

      if !min_time > (e -. s) then (
        min_time := e -. s;
        Printf.printf "%d,%06f\r" i !min_time;
        flush stdout;
      );
    done;

    Printf.fprintf fh "%d,%06f\n" i !min_time;
  done;

  close_out fh


let timing5 () =
  let lexer = Parser.start Lexer.token (Lexing.from_string "(x:a^16)*") in
  let lexer, varmap = Pattern.number_pattern lexer in

  let lexer = Language.compute_nullable_pat lexer in

  let (nfa, start) = Nfa.build varmap lexer in

  let fh = open_out "astarstar.log" in

  for i = 1 to 50 do
    let input = String.make (i * 1024) 'a' in

    let min_time = ref 1000.0 in

    for t = 1 to 100 do
      Gc.compact ();
      let s = Unix.gettimeofday () in
      ignore (Nfa.run (nfa, start) varmap input);
      let e = Unix.gettimeofday () in

      if !min_time > (e -. s) then (
        min_time := e -. s;
        Printf.printf "%d,%06f\r" i !min_time;
        flush stdout;
      );
    done;

    Printf.fprintf fh "%d,%06f\n" i !min_time;
  done;

  close_out fh


let timing6 () =
  let fh = open_out "dfa-exp.log" in

  for i = 1 to 50 do
    let pattern = Printf.sprintf "(x:(a+b)b(a+b)^%d)" i in

    let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let lexer = Language.compute_nullable_pat lexer in

    let min_time = ref 1000.0 in

    for t = 1 to 200 do
      Gc.compact ();
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
  done;

  close_out fh


let timing7 () =
  let fh = open_out "dfa-exp-states.log" in

  for i = 1 to 50 do
    let pattern = Printf.sprintf "(x:\
    ((a+b)*a(a+b)^%d) &\
    ((~((~a)+(~b)))*a(a+b)^%d) )" i i in

    let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
    let lexer, varmap = Pattern.number_pattern lexer in

    let lexer = Language.compute_nullable_pat lexer in

    let (nfa, start) = Nfa.build varmap lexer in

    Printf.printf "%d,%d\n" i (Hashtbl.length nfa);
    flush stdout;

    Printf.fprintf fh "%d,%d\n" i (Hashtbl.length nfa);
  done;

  close_out fh


let timing8 () =
  let pattern =
    ""
  in

  let lexer = Parser.start Lexer.token (Lexing.from_string pattern) in
  let lexer, varmap = Pattern.number_pattern lexer in

  let lexer = Language.compute_nullable_pat lexer in

  let min_time = ref 1000.0 in

  for t = 1 to 200 do
    Gc.compact ();
    let s = Unix.gettimeofday () in
    ignore (Nfa.build varmap lexer);
    let e = Unix.gettimeofday () in

    if !min_time > (e -. s) then (
      min_time := e -. s;
      Printf.printf "%06f\r" !min_time;
      flush stdout;
    );
  done



let timing () =
  timing2 ();
  timing4 ();
  timing6 ();
  (*timing8 ();*)
;;

(*let () = main ()*)
(*let () = timing ()*)

(*let () = timing1 ()*)
(*let () = timing3 ()*)
(*let () = timing5 ()*)
(*let () = timing7 ()*)


open Ast


let rec contains_binding = function
  | Eof  -> false
  | Char _ -> false
  | Sequence l -> List.exists contains_binding l
  | Alternation l -> List.exists contains_binding l
  | CharClass c -> false
  | Star re -> contains_binding re
  | Binding _ -> true
  | _ -> assert false


let rec extract_regexp = function
  | Eof -> Epsilon
  | Char chr -> Letter (Sloc.value chr)
  | Sequence [] ->
      Epsilon
  | Sequence l ->
      BatList.reduce
        (fun l r -> Concat (Maybe, r, l))
        (List.rev_map extract_regexp l)
  | Alternation [] -> failwith "Empty Alternation"
  | Alternation l ->
      BatList.reduce
        (fun l r -> Choice (Maybe, r, l))
        (List.rev_map extract_regexp l)
  | CharClass (Positive []) -> failwith "Empty CharClass"
  | CharClass (Positive l) ->
      BatList.reduce
        (fun l r -> Choice (Maybe, r, l))
        (List.sort (flip compare) (List.rev_map (function
          | Single chr -> Letter (Sloc.value chr)
          | _ -> assert false
        ) l))
  | Star re ->
      Types.Star (extract_regexp re)

  | _ -> assert false


let rec extract_pattern = function
  | Sequence [] -> failwith "Empty Sequence"
  | Sequence l ->
      BatList.reduce
        (fun l r -> PatConcat (Maybe, r, l))
        (List.rev_map extract_pattern l)
  | Alternation [] -> failwith "Empty Alternation"
  | Alternation l ->
      BatList.reduce
        (fun l r -> PatChoice (Maybe, r, l))
        (List.rev_map extract_pattern l)
  | Star re ->
      PatStar (extract_pattern re)
  | Binding (re, name) ->
      (*Printf.printf "%s\ncontains binding: %s\n"*)
        (*(Sexplib.Sexp.to_string_hum (sexp_of_regexp re))*)
        (*(string_of_bool (contains_binding re));*)
      if contains_binding re then
        VarGroup (Maybe, Sloc.value name, extract_pattern re)
      else
        VarBase (Maybe, Sloc.value name, extract_regexp re)

  | _ -> assert false


let make_name =
  let next = ref 0 in
  fun () ->
    incr next;
    string_of_int !next


let extract_rules = function
  | Rule (re, code) ->
      (*Printf.printf "%s\ncontains binding: %s\n"*)
        (*(Sexplib.Sexp.to_string_hum (sexp_of_regexp re))*)
        (*(string_of_bool (contains_binding re));*)
      if contains_binding re then
        (*VarGroup (Maybe, make_name (), extract_pattern re)*)
        VarGroup (Maybe, Sloc.value code, extract_pattern re)
      else
        (*VarBase (Maybe, make_name (), extract_regexp re)*)
        VarBase (Maybe, Sloc.value code, extract_regexp re)

let extract_rules_lexers = function
  | Lexer (_, _, rules) ->
      BatList.map extract_rules rules

let extract_rules_program = function
  | Program (_, _, [lexers], _) ->
      BatList.reduce
        (fun l r -> PatChoice (Maybe, r, l))
        (List.rev (extract_rules_lexers lexers))
  | _ -> assert false


let slurp ic =
  let lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file -> List.rev !lst


let timing9 () =
  (*Printexc.record_backtrace true;*)

  (*let file = "src/lang/dreml/testsuite/t0008.mll" in*)
  let file = "src/lang/dreml/testsuite/c_lexer.mll" in
  (*let file = "src/lang/dreml/testsuite/c_nosub.mll" in*)
  (*let input = "431ul" in*)
  let input =
    match Sys.argv with
    | [|_; input|] -> input
    | _ ->
        (*"int main() { return 3.0fl; }";*)
        let fh = open_in "wip/nsHTMLEditRules.i" in
        let input = String.concat "\n" (slurp fh) in
        close_in fh;
        input
  in

  let program = Parse.program_from_file file in
  let program = Resolve.resolve program in
  let program = SimplifyLex.simplify program in

  let lexer = extract_rules_program program in
  (*let lexer = PatStar lexer in*)

  Printf.printf "Lexer: %s\n"
    (Print.string_of_pattern CorePervasives.identity lexer);

  let lexer, varmap = Pattern.number_pattern lexer in

  let lexer = Language.compute_nullable_pat lexer in

  let do_timing = false in

  if do_timing then begin
    let min_time = ref 1000.0 in

    for t = 1 to 20 do
      Gc.compact ();
      let s = Unix.gettimeofday () in
      ignore (Nfa.build varmap lexer);
      let e = Unix.gettimeofday () in

      if !min_time > (e -. s) then (
        min_time := e -. s;
        Printf.printf "Construction: %06f\r" !min_time;
        flush stdout;
      );
    done;

    print_newline ();
  end;

  let (nfa, start) = Nfa.build varmap lexer in
  Printf.printf "%d states\n" (Hashtbl.length nfa);
  flush stdout;

  if do_timing then begin
    for i = 1 to 20 do
      let min_time = ref 1000.0 in

      let data = ref "" in
      for j = 1 to i do
        data := !data ^ input;
      done;
      let input = !data in

      for t = 1 to 20 do
        Gc.compact ();
        let s = Unix.gettimeofday () in
        ignore (Nfa.run_loop 0 (nfa, start) varmap input);
        let e = Unix.gettimeofday () in

        if !min_time > (e -. s) then (
          min_time := e -. s;
          Printf.printf "%d,%06f\r" (String.length input) !min_time;
          flush stdout;
        );
      done;

      print_newline ();
    done;
  end;

  let states = Nfa.run (nfa, start) varmap input in
  if true then (
    print_endline "after first step:";
    Nfa.Debug.show varmap input states
  );

  let states = Nfa.run (nfa, start) varmap input in
  if true then (
    print_endline "after second step:";
    Nfa.Debug.show varmap input states
  );

  let states = Nfa.run (nfa, start) varmap input in
  if true then (
    print_endline "after third step:";
    Nfa.Debug.show varmap input states
  );

  Nfa.run_loop 0 (nfa, start) varmap input


let () =
  (*Printexc.record_backtrace true;*)

  (*let file = "src/lang/dreml/testsuite/t0008.mll" in*)
  let file = "src/lang/dreml/testsuite/c_lexer.mll" in
  (*let file = "src/lang/dreml/testsuite/c_nosub.mll" in*)
  (*let input = "431ul" in*)
  let input =
    match Sys.argv with
    | [|_; input|] -> input
    | _ ->
        (*"int main() { return 3.0fl; }";*)
        let fh = open_in "wip/nsHTMLEditRules.i" in
        let input = String.concat "\n" (slurp fh) in
        close_in fh;
        input
  in

  let program = Parse.program_from_file file in
  let program = Resolve.resolve program in
  let program = SimplifyLex.simplify program in

  let lexer = extract_rules_program program in
  (*let lexer = PatStar lexer in*)

  Printf.printf "Lexer: %s\n"
    (Print.string_of_pattern CorePervasives.identity lexer);

  let lexer, varmap = Pattern.number_pattern lexer in

  let lexer = Language.compute_nullable_pat lexer in

  let (nfa, start) = Nfa.build varmap lexer in
  Printf.printf "%d states\n" (Hashtbl.length nfa);
  flush stdout;

  let nfa = Nfa.optimised (nfa, start) in

  Nfa.sexp_of_env_nfa nfa
  |> Sexplib.Sexp.output_hum stdout;

  Nfa.run_loop_opt 0 nfa varmap input
