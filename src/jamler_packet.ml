open Process
module Buffer = Jamler_buffer

type header =
  | BE1
  | BE2
  | BE4

type t = {mutable header : header;
	  mutable len : int;
	  buf : Buffer.t}

let create header =
  let header =
    match header with
      | `BE1 -> BE1
      | `BE2 -> BE2
      | `BE4 -> BE4
  in
    {header;
     len = -1;
     buf = Buffer.create 32}

let change st header =
  st.header <- header;
  st.len <- -1

let header_len =
  function
    | BE1 -> 1
    | BE2 -> 2
    | BE4 -> 4

exception Size_limit

let parse_len st h pos =
  match st.header with
    | BE1 ->
	Char.code h.[pos]
    | BE2 ->
	(Char.code h.[pos] lsl 8) lor Char.code h.[pos + 1]
    | BE4 ->
	let h0 = Char.code h.[pos] in
	let h1 = Char.code h.[pos + 1] in
	let h2 = Char.code h.[pos + 2] in
	let h3 = Char.code h.[pos + 3] in
	  if h0 > 0 then raise Size_limit;
	  (((((h0 lsl 8) lor h1) lsl 8) lor h2) lsl 8) lor h3

let parse_with_len st =
  let hlen = header_len st.header in
    if Buffer.length st.buf >= hlen + st.len then (
      let packet = Buffer.sub st.buf hlen st.len in
	Buffer.remove st.buf (hlen + st.len);
	st.len <- -1;
	Some packet
    ) else None

let parse st data =
  Buffer.add_string st.buf data;
  let hlen = header_len st.header in
    if st.len < 0 then (
      if Buffer.length st.buf >= hlen then (
	st.len <- parse_len st st.buf.Buffer.buf st.buf.Buffer.start;
	parse_with_len st
      ) else None
    ) else parse_with_len st


let free _st = ()

let decorate header packet =
  let len = String.length packet in
    match header with
      | `BE1 ->
	  String.make 1 (Char.chr (len land 0xff)) ^ packet
      | `BE2 ->
	  let h = String.make 2 '\000' in
	    h.[0] <- Char.chr ((len lsr 8) land 0xff);
	    h.[1] <- Char.chr (len land 0xff);
	    h ^ packet
      | `BE4 ->
	  let h = String.make 4 '\000' in
	    h.[0] <- Char.chr ((len lsr 24) land 0xff);
	    h.[1] <- Char.chr ((len lsr 16) land 0xff);
	    h.[2] <- Char.chr ((len lsr 8) land 0xff);
	    h.[3] <- Char.chr (len land 0xff);
	    h ^ packet

