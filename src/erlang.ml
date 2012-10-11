type reference = string * string
type pid = string * int * int

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

let node_of_pid (node, _, _) = node

let make_pid node id creation = (node, id, creation)

let term_to_string term =
  let rec aux b term =
    match term with
      | ErlInt x -> Buffer.add_string b (string_of_int x)
      | ErlFloat x -> Buffer.add_string b (string_of_float x)
      | ErlAtom x ->
	  Buffer.add_string b "\'";
	  Buffer.add_string b x;
	  Buffer.add_string b "\'";
      | ErlReference (node, x) ->
	  Buffer.add_string b "#Ref<";
	  Buffer.add_string b (Printf.sprintf "%S" x);
	  Buffer.add_string b ">";
      | ErlPid (node, id, creation) ->
	  Buffer.add_string b "<";
	  Buffer.add_string b (Printf.sprintf "%S" node);
	  Buffer.add_string b ".";
	  Buffer.add_string b (string_of_int id);
	  Buffer.add_string b ".";
	  Buffer.add_string b (string_of_int creation);
	  Buffer.add_string b ">";
      | ErlTuple xs ->
	  Buffer.add_string b "{";
	  for i = 0 to Array.length xs - 1 do
	    if i > 0
	    then Buffer.add_string b ", ";
	    aux b xs.(i)
	  done;
	  Buffer.add_string b "}";
      | ErlNil -> Buffer.add_string b "[]"
      | ErlCons (x, t) ->
	  Buffer.add_string b "[";
	  aux b x;
	  let u = ref t in
	    while (match !u with
		     | ErlCons (x, t) ->
			 Buffer.add_string b ", ";
			 aux b x;
			 u := t;
			 true
		     | ErlNil ->
			 false
		     | t ->
			 Buffer.add_string b " | ";
			 aux b t;
			 false
		  ) do
	      ()
	    done;
	    Buffer.add_string b "]";
      | ErlString x ->
	  Buffer.add_string b (Printf.sprintf "%S" x);
      | ErlBinary x ->
	  Buffer.add_string b "<<";
	  Buffer.add_string b (Printf.sprintf "%S" x);
	  Buffer.add_string b ">>"
  in
  let b = Buffer.create 10 in
    aux b term;
    Buffer.contents b

(* TODO: move somewhere *)
let add_int16_be buf x =
  Buffer.add_char buf (Char.chr ((x lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (x land 0xff))

let add_int32_be buf x =
  Buffer.add_char buf (Char.chr ((x lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((x lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((x lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (x land 0xff))

let term_to_buffer b term =
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
	    for i = 7 downto 0 do
	      add_byte x (i * 8)
	    done
      | ErlAtom x ->
	  let len = String.length x in
	    assert (len < 256);
	    Buffer.add_char b '\100';
	    Buffer.add_char b '\000';
	    Buffer.add_char b (Char.chr len);
	    Buffer.add_string b x;
      | ErlReference (node, opaque) ->
	  Buffer.add_char b '\114';
	  let n = String.length opaque / 4 in
	    add_int16_be b n;
	    aux b (ErlAtom node);
	    let creation = 0 in		(* TODO *)
	      Buffer.add_char b (Char.chr creation);
	      Buffer.add_string b opaque
      | ErlPid (node, id, serial) ->
	  Buffer.add_char b '\103';
	  aux b (ErlAtom node);
	  add_int32_be b id;
	  add_int32_be b serial;
	  let creation = 0 in		(* TODO *)
	    Buffer.add_char b (Char.chr creation);
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
    Buffer.add_char b '\131';
    aux b term

let term_to_binary term =
  let b = Buffer.create 10 in
    term_to_buffer b term;
    Buffer.contents b

let binary_to_term s pos =
  let pos = ref pos in
  let len = String.length s in
    if !pos < len && s.[!pos] = '\131' then (
      incr pos;
      let parse_int32_to_int s pos =
	if !pos + 3 < len then (
	  let x0 = Char.code s.[!pos] in
	  let x1 = Char.code s.[!pos + 1] in
	  let x2 = Char.code s.[!pos + 2] in
	  let x3 = Char.code s.[!pos + 3] in
	  let x0 = if x0 < 128 then x0 else x0 - 256 in
	  let x =
	    (((((x0 lsl 8) lor x1) lsl 8) lor x2) lsl 8) lor x3
	  in
	    pos := !pos  + 4;
	    x
	) else invalid_arg "binary_to_term"
      in
      let parse_int24_to_int s pos =
	if !pos + 3 < len then (
	  let x0 = Char.code s.[!pos] in
	  let x1 = Char.code s.[!pos + 1] in
	  let x2 = Char.code s.[!pos + 2] in
	  let x3 = Char.code s.[!pos + 3] in
	  let x =
	    (((((x0 lsl 8) lor x1) lsl 8) lor x2) lsl 8) lor x3
	  in
	    if x0 <> 0 then invalid_arg "binary_to_term";
	    pos := !pos  + 4;
	    x
	) else invalid_arg "binary_to_term"
      in
      let rec parse s pos =
	let len = String.length s in
	  if !pos < len then (
	    match s.[!pos] with
	      | '\097' ->
		  incr pos;
		  if !pos < len then (
		    let res = ErlInt (Char.code s.[!pos]) in
		      incr pos;
		      res
		  ) else invalid_arg "binary_to_term"
	      | '\098' ->
		  incr pos;
		  ErlInt (parse_int32_to_int s pos)
	      | '\110' ->
		  incr pos;
		  if !pos < len then (
		    let n = Char.code s.[!pos] in
		      incr pos;
		      if !pos + n < len then (
			let sign = s.[!pos] <> '\000' in
			  incr pos;
			  let res = ref 0 in
			    for i = 0 to n - 1 do
			      res :=
				!res lor ((Char.code s.[!pos + i]) lsl (i * 8))
			    done;
			    pos := !pos  + n;
			    if sign
			    then ErlInt (- !res)
			    else ErlInt !res
		      ) else invalid_arg "binary_to_term"
		  ) else invalid_arg "binary_to_term"
	      | '\070' ->
		  incr pos;
		  if !pos + 7 < len then (
		    let x = ref 0L in
		      for i = 0 to 7 do
			x :=
			  Int64.logor
			    (Int64.shift_left !x 8)
			    (Int64.of_int (Char.code s.[!pos + i]))
		      done;
		      pos := !pos + 8;
		      ErlFloat (Int64.float_of_bits !x)
		  ) else invalid_arg "binary_to_term"
	      | '\100' ->
		  incr pos;
		  if !pos + 1 < len then (
		    if s.[!pos] <> '\000' then invalid_arg "binary_to_term";
		    let n = Char.code s.[!pos + 1] in
		      pos := !pos + 2;
		      if !pos + n - 1 < len then (
			let x = String.sub s !pos n in
			  pos := !pos + n;
			  ErlAtom x
		      ) else invalid_arg "binary_to_term"
		  ) else invalid_arg "binary_to_term"
	      | '\104' ->
		  incr pos;
		  if !pos < len then (
		    let n = Char.code s.[!pos] in
		      incr pos;
		      let xs = Array.make n ErlNil in
			for i = 0 to n - 1 do
			  xs.(i) <- parse s pos
			done;
			ErlTuple xs
		  ) else invalid_arg "binary_to_term"
	      | '\105' ->
		  incr pos;
		  let n = parse_int24_to_int s pos in
		  let xs = Array.make n ErlNil in
		    for i = 0 to n - 1 do
		      xs.(i) <- parse s pos
		    done;
		    ErlTuple xs
	      | '\106' ->
		  incr pos;
		  ErlNil
	      | '\107' ->
		  incr pos;
		  if !pos + 1 < len then (
		    let n0 = Char.code s.[!pos] in
		    let n1 = Char.code s.[!pos + 1] in
		    let n = (n0 lsl 8) lor n1 in
		      pos := !pos + 2;
		      if !pos + n - 1 < len then (
			let x = String.sub s !pos n in
			  pos := !pos + n;
			  ErlString x
		      ) else invalid_arg "binary_to_term"
		  ) else invalid_arg "binary_to_term"
	      | '\108' ->
		  incr pos;
		  let n = parse_int24_to_int s pos in
		  let xs = Array.make n ErlNil in
		    for i = 0 to n - 1 do
		      xs.(i) <- parse s pos
		    done;
		    let tail = parse s pos in
		    let res = ref tail in
		      for i = n - 1 downto 0 do
			res := ErlCons (xs.(i), !res)
		      done;
		      !res
	      | '\109' ->
		  incr pos;
		  let n = parse_int24_to_int s pos in
		    if !pos + n - 1 < len then (
		      let x = String.sub s !pos n in
			pos := !pos + n;
			ErlBinary x
		    ) else invalid_arg "binary_to_term"
	      | '\103' ->
		  incr pos;
		  let node =
		    match parse s pos with
		      | ErlAtom node -> node
		      | _ -> invalid_arg "binary_to_term"
		  in
		  let id = parse_int24_to_int s pos in
		  let serial = parse_int24_to_int s pos in
		    if !pos < len then (
		      let _creation = Char.code s.[!pos] in
			pos := !pos + 1;
			ErlPid (node, id, serial)
		    ) else invalid_arg "binary_to_term"
	      | '\114' ->
		  incr pos;
		  if !pos + 1 < len then (
		    let n0 = Char.code s.[!pos] in
		    let n1 = Char.code s.[!pos + 1] in
		    let n = (n0 lsl 8) lor n1 in
		      pos := !pos + 2;
		      let node =
			match parse s pos with
			  | ErlAtom node -> node
			  | _ -> invalid_arg "binary_to_term"
		      in
			if !pos + 4 * n < len then (
			  let opaque = String.sub s (!pos + 1) (4 * n) in
			    pos := !pos + 1 + 4 * n;
			    ErlReference (node, opaque)
			) else invalid_arg "binary_to_term"
		  ) else invalid_arg "binary_to_term"
	      | _ ->
		  invalid_arg "binary_to_term"
	  ) else invalid_arg "binary_to_term"
      in
      let res = parse s pos in
	(res, !pos)
    ) else invalid_arg "binary_to_term"

let jid_to_term jid =
  ErlTuple [| ErlAtom "jid";
	      ErlBinary jid.Jlib.user;
	      ErlBinary jid.Jlib.server;
	      ErlBinary jid.Jlib.resource;
	      ErlBinary (jid.Jlib.luser :> string);
	      ErlBinary (jid.Jlib.lserver :> string);
	      ErlBinary (jid.Jlib.lresource :> string);
	   |]


(* based on http://okmij.org/ftp/ML/first-class-modules/generics.ml *)
module ErlType =
struct
  module type Interpretation =
  sig
    type 'a tc

    val int : int tc
    val string : string tc
    val binary : string tc
    val atom : string tc

    val xml : Xml.element tc

    val ( * ) : 'a tc -> 'b tc -> ('a * 'b) tc

    val list : 'a tc -> 'a list tc
  end

  module type Repr =
  sig
    type a
    module Interpret (I : Interpretation) :
    sig
      val result : a I.tc
    end
  end

  type 'a repr = (module Repr with type a = 'a)

  let int : int repr =
    (module
     struct
       type a = int
       module Interpret (I : Interpretation) =
       struct
         let result = I.int
       end
     end : Repr with type a = int)

  let string : string repr =
    (module
     struct
       type a = string
       module Interpret (I : Interpretation) =
       struct
         let result = I.string
       end
     end : Repr with type a = string)

  let binary : string repr =
    (module
     struct
       type a = string
       module Interpret (I : Interpretation) =
       struct
         let result = I.binary
       end
     end : Repr with type a = string)

  let atom : string repr =
    (module
     struct
       type a = string
       module Interpret (I : Interpretation) =
       struct
         let result = I.atom
       end
     end : Repr with type a = string)

  let xml : Xml.element repr =
    (module
     struct
       type a = Xml.element
       module Interpret (I : Interpretation) =
       struct
         let result = I.xml
       end
     end : Repr with type a = Xml.element)

  let ( * ) : 'a 'b. 'a repr -> 'b repr -> ('a * 'b) repr
    = fun (type a') (type b') arepr brepr ->
      (module
       struct
         type a = a' * b'
         module A = (val arepr : Repr with type a = a')
         module B = (val brepr : Repr with type a = b')
         module Interpret (I : Interpretation) =
         struct
           module AI = A.Interpret(I)
           module BI = B.Interpret(I)
           open I
           let result = AI.result * BI.result
         end
       end : Repr with type a = a' * b')

  let list : 'a. 'a repr -> 'a list repr
    = fun (type a') arepr ->
      (module
       struct
         type a = a' list
         module A = (val arepr : Repr with type a = a')
         module Interpret (I : Interpretation) =
         struct
           module AI = A.Interpret(I)
           open I
           let result = list AI.result
         end
       end : Repr with type a = a' list)


  module FromTerm
    : Interpretation with type 'a tc = erl_term -> 'a =
  struct
    type 'a tc = erl_term -> 'a

    let int =
      function
	| ErlInt x -> x
	| _ -> invalid_arg "from_term"

    let string =
      function
	| ErlString x -> x
	| ErlNil -> ""
	| _ -> invalid_arg "from_term"

    let binary =
      function
	| ErlBinary x -> x
	| _ -> invalid_arg "from_term"

    let atom =
      function
	| ErlAtom x -> x
	| _ -> invalid_arg "from_term"

    let ( * ) f g =
      function
	| ErlTuple [| a; b |] ->
	    (f a, g b)
	| _ -> invalid_arg "from_term"

    let rec list f =
      function
	| ErlNil -> []
	| ErlCons (x, t) ->
	    f x :: list f t
	| _ -> invalid_arg "from_term"


    let rec iolist' b =
      function
	| ErlNil -> ()
	| ErlString x
	| ErlBinary x -> Buffer.add_string b x
	| ErlInt x when x >= 0 && x < 256 -> Buffer.add_char b (Char.chr x)
	| ErlCons (x, t) ->
	    iolist' b x;
	    iolist' b t
	| _ -> invalid_arg "from_term"

    let iolist =
      function
	| ErlNil -> ""
	| ErlString x
	| ErlBinary x -> x
	| (ErlCons _) as x ->
	    let b = Buffer.create 10 in
	      iolist' b x;
	      Buffer.contents b
	| _ -> invalid_arg "from_term"

    let attrs_type = list (binary * binary)

    let rec xml_cdata =
      function
	| ErlTuple [| ErlAtom "xmlel";
		      ErlBinary name;
		      attrs;
		      els |] ->
	    let attrs = attrs_type attrs in
	    let els = list xml_cdata els in
	      `XmlElement (name, attrs, els)
	| ErlTuple [| ErlAtom "xmlcdata";
		      cdata |] ->
	    `XmlCdata (iolist cdata)
	| _ -> invalid_arg "from_term"

    let xml =
      function
	| ErlTuple [| ErlAtom "xmlel";
		      ErlBinary name;
		      attrs;
		      subels |] ->
	    let attrs = attrs_type attrs in
	    let subels = list xml_cdata subels in
	      `XmlElement (name, attrs, subels)
	| _ -> invalid_arg "from_term"

  end

  let from_term : 'a . 'a repr -> erl_term -> 'a =
    fun (type a) repr term -> 
      let module R = (val repr : Repr with type a = a) in 
      let module N = R.Interpret (FromTerm) in
	N.result term

  module ToTerm
    : Interpretation with type 'a tc = 'a -> erl_term =
  struct
    type 'a tc = 'a -> erl_term

    let int x = ErlInt x
    let string x = ErlString x
    let binary x = ErlBinary x
    let atom x = ErlAtom x

    let ( * ) f g (a, b) = ErlTuple [| f a; g b |]

    let list f xs =
      List.fold_right
	(fun x t -> ErlCons (f x, t)) xs ErlNil

    let attrs_type = list (binary * binary)

    let rec xml_cdata =
      function
	| `XmlElement (name, attrs, els) ->
	    ErlTuple [| ErlAtom "xmlel";
			ErlBinary name;
			attrs_type attrs;
			list xml_cdata els |]
	| `XmlCdata x ->
	    ErlTuple [| ErlAtom "xmlcdata";
			ErlBinary x |]

    let xml el = xml_cdata (el :> Xml.element_cdata)

  end

  let to_term : 'a . 'a repr -> 'a -> erl_term =
    fun (type a) repr term -> 
      let module R = (val repr : Repr with type a = a) in 
      let module N = R.Interpret (ToTerm) in
	N.result term

end


let term_to_route =
  function
    | ErlTuple [| ErlAtom "route";
		  ErlTuple [| ErlAtom "jid";
			      from_user; from_server; from_resource;
			      _; _; _ |];
		  ErlTuple [| ErlAtom "jid";
			      to_user; to_server; to_resource;
			      _; _; _ |];
		  msg |] -> (
	let from_user = ErlType.(from_term binary from_user) in
	let from_server = ErlType.(from_term binary from_server) in
	let from_resource = ErlType.(from_term binary from_resource) in
	let to_user = ErlType.(from_term binary to_user) in
	let to_server = ErlType.(from_term binary to_server) in
	let to_resource = ErlType.(from_term binary to_resource) in
	let from = Jlib.make_jid_exn from_user from_server from_resource in
	let to' = Jlib.make_jid_exn to_user to_server to_resource in
	let msg = ErlType.(from_term xml msg) in
	  `Route (from, to', msg)
      )
    | _ -> invalid_arg "route_from_term"

