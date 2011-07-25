type reference
type pid

type erl_term =
  | ErlInt of int
  | ErlFloat of float
  | ErlAtom of string
  | ErlReference of reference
  (*| ErlPort of*)
  | ErlPid of pid
  | ErlTuple of erl_term array
  | ErlNil
  | ErlString of string
  | ErlCons of erl_term * erl_term
  | ErlBinary of string
  (*| ErlBigInt of*)
  (*| ErlFun*)


let rec term_to_string =
  function
    | ErlInt x -> string_of_int x
    | ErlFloat x -> string_of_float x
    | ErlAtom x -> x
    | ErlReference _x -> "#ref"
    | ErlPid _x -> "#pid"
    | ErlTuple xs ->
	"{" ^
	  String.concat ", " (List.map term_to_string (Array.to_list xs)) ^
	  "}"
    | ErlNil -> "[]"
    | ErlCons (x, t) -> "[" ^ term_to_string x ^ " | " ^ term_to_string t ^ "]"
    | ErlString x -> "\"" ^ x ^ "\""
    | ErlBinary x -> "<<\"" ^ x ^ "\">>"

let term_to_binary term =
  let rec aux b =
    function
      | ErlInt x ->
	  if x >= 0 && x < 256 then (
	    Buffer.add_char b '\097';
	    Buffer.add_char b (Char.chr x);
	  ) else if Nativeint.size = 32 || (x >= ((-1) lsl 31) && x < (1 lsl 31))
	  then (
	    Buffer.add_char b '\098';
	    Buffer.add_char b (Char.chr ((x lsr 24) land 0xff));
	    Buffer.add_char b (Char.chr ((x lsr 16) land 0xff));
	    Buffer.add_char b (Char.chr ((x lsr 8) land 0xff));
	    Buffer.add_char b (Char.chr (x land 0xff));
	  ) else (
	    Buffer.add_char b '\110';
	    let s = if x >= 0 then 0 else 1 in
	    let x = if x >= 0 then x else -x in
	    let n = ref 0 in
	    let y = ref x in
	    let () =
	      while !y > 0 do
		y := !y lsr 8;
		incr n;
	      done
	    in
	    let n = !n in
	      Buffer.add_char b (Char.chr n);
	      Buffer.add_char b (Char.chr s);
	      for i = 0 to n - 1 do
		let c = (x lsr (i * 8)) land 0xff in
		  Buffer.add_char b (Char.chr c);
	      done
	  )
      | ErlFloat x ->
	  Buffer.add_char b '\070';
	  let x = Int64.bits_of_float x in
	  let add_byte x shift =
	    Buffer.add_char b
	      (Char.chr (Int64.to_int (Int64.shift_right x shift) land 0xff))
	  in
	    add_byte x 56;
	    add_byte x 48;
	    add_byte x 40;
	    add_byte x 32;
	    add_byte x 24;
	    add_byte x 16;
	    add_byte x 8;
	    add_byte x 0;
      | ErlAtom x ->
	  let len = String.length x in
	    assert (len < 256);
	    Buffer.add_char b '\100';
	    Buffer.add_char b '\000';
	    Buffer.add_char b (Char.chr len);
	    Buffer.add_string b x;
      | ErlReference _x ->
	  (* TODO *)
	  assert false
      | ErlPid _x ->
	  (* TODO *)
	  assert false
      | ErlTuple xs ->
	  let len = Array.length xs in
	    if len < 256 then (
	      Buffer.add_char b '\104';
	      Buffer.add_char b (Char.chr len);
	      for i = 0 to len - 1 do
		aux b xs.(i)
	      done
	    ) else (
	      Buffer.add_char b '\105';
	      Buffer.add_char b (Char.chr ((len lsr 24) land 0xff));
	      Buffer.add_char b (Char.chr ((len lsr 16) land 0xff));
	      Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
	      Buffer.add_char b (Char.chr (len land 0xff));
	      for i = 0 to len - 1 do
		aux b xs.(i)
	      done
	    )
      | ErlNil ->
	  Buffer.add_char b '\106';
      | ErlCons (_x, _t) as t ->
	  let u = ref t in
	  let len = ref 0 in
	  let _ =
	    while (match !u with ErlCons (_, t) -> u := t; true | _ -> false) do
	      incr len
	    done
	  in
	  let u = ref t in
	  let len = !len in
	    Buffer.add_char b '\108';
	    Buffer.add_char b (Char.chr ((len lsr 24) land 0xff));
	    Buffer.add_char b (Char.chr ((len lsr 16) land 0xff));
	    Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
	    Buffer.add_char b (Char.chr (len land 0xff));
	    for i = 0 to len - 1 do
	      match !u with
		| ErlCons (x, t) ->
		    aux b x;
		    u := t
		| _ -> assert false
	    done;
	    aux b !u
      | ErlString x ->
	  let len = String.length x in
	    if len = 0 then (
	      Buffer.add_char b '\106';
	    ) else if len < 65536 then (
	      Buffer.add_char b '\107';
	      Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
	      Buffer.add_char b (Char.chr (len land 0xff));
	      Buffer.add_string b x;
	    ) else (
	      Buffer.add_char b '\108';
	      Buffer.add_char b (Char.chr ((len lsr 24) land 0xff));
	      Buffer.add_char b (Char.chr ((len lsr 16) land 0xff));
	      Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
	      Buffer.add_char b (Char.chr (len land 0xff));
	      for i = 0 to len - 1 do
		Buffer.add_char b '\097';
		Buffer.add_char b x.[i];
	      done;
	      Buffer.add_char b '\106';
	    )
      | ErlBinary x ->
	  let len = String.length x in
	    Buffer.add_char b '\109';
	    Buffer.add_char b (Char.chr ((len lsr 24) land 0xff));
	    Buffer.add_char b (Char.chr ((len lsr 16) land 0xff));
	    Buffer.add_char b (Char.chr ((len lsr 8) land 0xff));
	    Buffer.add_char b (Char.chr (len land 0xff));
	    Buffer.add_string b x;
  in
  let b = Buffer.create 10 in
    Buffer.add_char b '\131';
    aux b term;
    Buffer.contents b
