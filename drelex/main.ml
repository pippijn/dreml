open Types


let a =
  VarBase (
    Fun (fun pos -> "got 'a'"),
    Letter 'a'
  )

let b =
  VarBase (
    Fun (fun pos -> "got 'b'"),
    Letter 'b'
  )

let choice =
  VarGroup (
    Fun (fun pos a -> 3),
    PatChoice (a, b)
  )

let concat =
  VarGroup (
    Fun (fun pos (a, b) -> "concat got " ^ a ^ " and " ^ b),
    PatConcat (a, b)
  )

let lexer =
  VarGroup (
    Fun (fun pos (a, b) -> "parent got [" ^ string_of_int a ^ ", " ^ b ^ "]"),
    PatConcat (choice, concat)
  )


let () =
  print_endline (Print.string_of_pattern lexer)
