open Types
open Ops


let graphviz_of_nfa out (nfa, start) =
  output_string out "digraph G {\n";
  output_string out "  rankdir = LR;\n";

  (* final states *)
  output_string out "  node [shape = doublecircle];";
  Hashtbl.iter (fun from targets ->
    if Language.nullable (Language.regex_of_pattern from) then
      Printf.fprintf out "\n  \"%s\"" (Util.string_of_pattern from)
  ) nfa;
  output_string out ";\n  node [shape = circle];\n";

  (* edges *)
  Hashtbl.iter (fun from targets ->
    Array.iteri (fun ch pds ->
      let chr = Char.escaped (Char.chr ch) in
      List.iter (fun (pd, _) ->
        Printf.fprintf out "  \"%s\" -> \"%s\" [ label = \"%s\" ];\n"
          (Util.string_of_pattern from)
          (Util.string_of_pattern pd)
          chr
      ) pds
    ) targets
  ) nfa;

  output_string out "}\n";
;;
