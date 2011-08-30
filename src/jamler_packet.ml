open Process
module Buf = Jamler_buffer

type msg = [ `Packet of string ]

type header =
  | BE1
  | BE2
  | BE4

type t = {pid : msg pid;
	  header : header;
	  mutable len : int;
	  buf : Buffer.t}

let create pid header =
  let pid = (pid :> msg pid) in
  let header =
    match header with
      | `BE1 -> BE1
      | `BE2 -> BE2
      | `BE4 -> BE4
  in
    {pid;
     header;
     len = -1;
     buf = Buffer.create 32}

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

let rec parse_string st s pos =
  let hlen = header_len st.header in
    if String.length s >= pos + hlen then (
      let len = parse_len st s pos in
	if String.length s >= pos + hlen + len then (
	  let packet = String.sub s (pos + hlen) len in
	    st.pid $! `Packet packet;
	    parse_string st s (pos + hlen + len)
	) else pos
    ) else pos

let parse_with_len st =
  let hlen = header_len st.header in
    if Buffer.length st.buf >= hlen + st.len then (
      let s = Buffer.contents st.buf in
      let pos = parse_string st s 0 in
	Buffer.reset st.buf;
	Buffer.add_substring st.buf s pos (String.length s - pos);
	st.len <- -1;
    )

let parse st data =
  Buffer.add_string st.buf data;
  let hlen = header_len st.header in
    if st.len < 0 then (
      if Buffer.length st.buf >= hlen then (
	let h = Buffer.sub st.buf 0 hlen in
	  st.len <- parse_len st h 0;
	  parse_with_len st
      )
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

