install Program ".DEFAULT" [
  (* Target *)
  Name		"drelex";

  (* Sources *)
  Modules [
    "ExprSets";
    "Instruction";
    "Language";
    "Lexer";
    "Main";
    "Nfa";
    "Parser";
    "Pattern";
    "Print";
    "Simplify";
    "Tag";
    "Transition";
    "Types";
    "Util";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "batteries";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "types.ml",		"-syntax camlp4o";
  ];

  Code "OCAMLOPT_FLAGS += -inline 0";
]
