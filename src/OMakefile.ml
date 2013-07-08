install Program ".DEFAULT" [
  (* Target *)
  Name		"dreml";

  (* Sources *)
  Modules [
    "Dreml";
    "Duplicates";
    "ExprSets";
    "Instruction";
    "Language";
    "Lexer";
    "Main";
    "Negation";
    "Nfa";
    "Parser";
    "Partial";
    "Pattern";
    "Simplify";
    "Transition";
    "Types";
    "Util";
    "Visualise";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "batteries.syntax";
    "deriving-ocsigen";
    "deriving-ocsigen.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "main.ml",		"-syntax camlp4o";
    "nfa.ml",		"-syntax camlp4o";
    "exprSets.ml",	"-syntax camlp4o";
    "partial.ml",	"-syntax camlp4o";
    "types.ml",		"-syntax camlp4o";
  ];
]
