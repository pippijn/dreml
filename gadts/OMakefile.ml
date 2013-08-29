install Program ".DEFAULT" [
  (* Target *)
  Name		"drelex-gadts";

  (* Sources *)
  Modules [
    "ExprSets";
    "Language";
    "Main";
    "Print";
    "Types";
    "Util";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "batteries";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "types.ml",		"-syntax camlp4o";
  ];
]
