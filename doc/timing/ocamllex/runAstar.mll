rule token = parse
"a"*	{ }

{
  let () =
    let fh = open_out "run-astar.log" in

    for i = 1 to 50 do
      let str = String.make (i * 1024) 'a' in

      let min_time = ref 1000.0 in

      for t = 1 to 200 do
	let lexbuf = Lexing.from_string str in
	Gc.compact ();
	let s = Unix.gettimeofday () in
	token lexbuf;
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
}
