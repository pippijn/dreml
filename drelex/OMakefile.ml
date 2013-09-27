install Program ".DEFAULT" [
  (* Target *)
  Name		"drelex";

  (* Sources *)
  Modules [
    "Ast";
    "CharClass";
    "ExprSets";
    "Instruction";
    "Language";
    "Lexer";
    "Main";
    "Nfa";
    "Olexer";
    "Oparser";
    "Parse";
    "Parser";
    "Pattern";
    "Print";
    "Resolve";
    "Simplify";
    "SimplifyLex";
    "Tag";
    "Transition";
    "Types";
    "Util";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "camlp4.fulllib";
    "codegen";
    "corelib";
    "batteries";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "ast.ml",		"-syntax camlp4o";
    "types.ml",		"-syntax camlp4o";
  ];

  Code "OCAMLOPT_FLAGS += -inline 0";
]
