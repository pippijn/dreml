val go = fn n =>
let
  val () = CalcLex.UserDeclarations.c := n
  val timer = Timer.startRealTimer ()
  val _ = CalcLex.makeLexer (CalcLex.UserDeclarations.foo) ()
  val time = Timer.checkRealTimer timer
  val time = Time.toMicroseconds time
in
  time
end

val min =
  case LargeInt.fromString "100000000" of
       NONE => ref 0
     | SOME n => ref n

val show = fn (n, time) =>
let
  val () = ()
in
  if time < !min then (
    min := time;
    TextIO.output (TextIO.stdOut, Int.toString (n * 4));
    TextIO.output (TextIO.stdOut, ",");
    TextIO.output (TextIO.stdOut, LargeInt.toString time);
    TextIO.output (TextIO.stdOut, "\n");
    ()
  ) else (
    ()
  )
end

val start = fn n =>
let
  val c = ref 1000
in
  while !c > 0 do (
    show (n, (go n))
  )
end
