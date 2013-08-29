open Types


let a : char pattern =
  VarBase (
    Fun (fun pos -> 'a'),
    Letter 'a'
  )

let b : char pattern =
  VarBase (
    Fun (fun pos -> 'b'),
    Letter 'b'
  )

let choice : int pattern =
  VarGroup (
    Fun (fun pos a -> Char.code a),
    PatChoice (a, b)
  )

let concat : string pattern =
  VarGroup (
    Fun (fun pos (a, b) -> "concat got " ^ Char.escaped a ^ " and " ^ Char.escaped b),
    PatConcat (a, b)
  )

let lexer : string pattern =
  VarGroup (
    Fun (fun pos (a, b) -> "parent got [" ^ string_of_int a ^ ", " ^ b ^ "]"),
    PatConcat (choice, concat)
  )


let () =
  print_endline (Print.string_of_pattern lexer)
