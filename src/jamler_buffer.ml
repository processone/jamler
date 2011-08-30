type t =
    {mutable buf : string;
     mutable start : int;
     mutable len : int;
     mutable size : int;
     initial_size : int;
    }

let create size =
  let buf = String.create size in
    {buf;
     start = 0;
     len = 0;
     size;
     initial_size = size;
    }

let contents b =
  String.sub b.buf b.start b.len

let reset b =
  b.buf <- String.create b.initial_size;
  b.start <- 0;
  b.len <- 0;
  b.size <- b.initial_size

let resize b min_size =
  let next_size = ref b.size in
    while !next_size < min_size do
      next_size := !next_size * 2 + 1
    done;
    if !next_size > Sys.max_string_length then (
      if min_size <= Sys.max_string_length
      then next_size := Sys.max_string_length
      else failwith "Jamler_buffer.resize: cannot grow buffer"
    );
    let buf = String.create !next_size in
      String.blit b.buf b.start buf 0 b.len;
      b.buf <- buf;
      b.start <- 0;
      b.size <- !next_size

let add_char b c =
  if b.start + b.len >= b.size
  then resize b (b.start + b.len + 1);
  b.buf.[b.start + b.len] <- c;
  b.len <- b.len + 1

let add_substring b s ofs len =
  if b.start + b.len + len > b.size
  then resize b (b.start + b.len + len);
  String.blit s ofs b.buf (b.start + b.len) len;
  b.len <- b.len + len

let add_string b s =
  add_substring b s 0 (String.length s)

let remove b len =
  if len > b.len
  then invalid_arg "Jamler_buffer.remove";
  b.start <- b.start + len;
  b.len <- b.len - len;
  if b.start > b.size lsr 1 then (
    String.blit b.buf b.start b.buf 0 b.len;
    b.start <- 0;
  )
