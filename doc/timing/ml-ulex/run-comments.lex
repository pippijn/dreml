%name CalcLex;

%defs (
type lex_result = string option
val eof = fn () => NONE

val s = "/* comment */"
val s = s ^ s (* 2 *)
val s = s ^ s (* 4 *)
val s = s ^ s (* 8 *)
val s = s ^ s (* 16 *)
val s = s ^ s (* 32 *)
val s = s ^ s (* 64 *)
val s = s ^ s (* 128 *)
val s = s ^ s (* 256 *)
val s = s ^ s (* 512 *)
val s = s ^ s (* 1024 *)
val s = s ^ s (* 2048 *)
val s = s ^ s (* 4096 *)
val s = s ^ s (* 8192 *)

val c = ref 1024

fun foo n = (
  c := !c - 1;
  if !c >= 0 then
    s
  else
    ""
)
);

"/*"(~(.*"*/".*))"*/"	=> (SOME ("success: " ^ yytext ^ "\n"));
.			=> (SOME ("fail: " ^ yytext ^ "\n"));
