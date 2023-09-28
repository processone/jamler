open Process

let src = Jamler_log.new_src "socket"

type mod_name = [ `Tcp | `SSL | `Zlib ]

let _ =
  (* TODO https://github.com/ocaml/opam-repository/issues/20524 *)
  (*Ssl.init ()*)
  ()

type tls_option = [ `Certfile of string | `Connect ]

(*
module SSLSocketMod : SocketMod with type t = Lwt_ssl.socket Lwt.t =
struct
  type t = Lwt_ssl.socket Lwt.t
  let read sock buf off len =
    let%lwt sock = sock in
      Lwt_ssl.read sock buf off len
  let write sock buf off len =
    let%lwt sock = sock in
      Lwt_ssl.write sock buf off len
  let close sock =
    let%lwt sock = sock in
      Lwt_ssl.close sock
  let get_fd _socket = assert false
  let name = `SSL
end
 *)

(*
module SSLSocketMod : SocketMod with type t = unit =
struct
  type t = unit
  let read _sock _buf _off _len = assert false
  let write _sock _buf _off _len = assert false
  let close _sock = assert false
  let get_fd _socket = assert false
  let name = `SSL
end
 *)

module Zlib =
struct
  open Cryptokit

  (* Copy&paste from cryptokit to enable zlib headers *)
class buffered_output initial_buffer_size =
  object(self)
    val mutable obuf = Bytes.create initial_buffer_size
    val mutable obeg = 0
    val mutable oend = 0

    method private ensure_capacity n =
      let len = Bytes.length obuf in
      if oend + n > len then begin
        if oend - obeg + n < len then begin
          Bytes.blit obuf obeg obuf 0 (oend - obeg);
          oend <- oend - obeg;
          obeg <- 0
        end else begin
          let newlen = ref (2 * len) in
          while oend - obeg + n > (!newlen) do
            newlen := (!newlen) * 2
          done;
          if (!newlen) > Sys.max_string_length then begin
            if (oend - obeg + n) <= Sys.max_string_length then
              newlen := Sys.max_string_length
            else
              raise (Error Output_buffer_overflow)
          end;
          let newbuf = Bytes.create (!newlen) in
          Bytes.blit obuf obeg newbuf 0 (oend - obeg);
          obuf <- newbuf;
          oend <- oend - obeg;
          obeg <- 0
        end
      end

    method available_output = oend - obeg

    method get_substring =
      let res = (obuf, obeg, oend - obeg) in obeg <- 0; oend <- 0; res

    method get_string =
      let res = Bytes.sub_string obuf obeg (oend - obeg) in obeg <- 0; oend <- 0; res

    method get_char =
      if obeg >= oend then raise End_of_file;
      let r = Bytes.get obuf obeg in
      obeg <- obeg + 1;
      r

    method get_byte =
      Char.code self#get_char

    method wipe =
      wipe_bytes obuf
  end


type stream
      
type flush_command =
    Z_NO_FLUSH
  | Z_SYNC_FLUSH
  | Z_FULL_FLUSH
  | Z_FINISH
      
external deflate_init: int -> bool -> stream = "caml_zlib_deflateInit"
external deflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "caml_zlib_deflate_bytecode" "caml_zlib_deflate"
external deflate_end: stream -> unit = "caml_zlib_deflateEnd"

external inflate_init: bool -> stream = "caml_zlib_inflateInit"
external inflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "caml_zlib_inflate_bytecode" "caml_zlib_inflate"
external inflate_end: stream -> unit = "caml_zlib_inflateEnd"
      
class compress level =
  object(self)
    val zs = deflate_init level true
      
    inherit buffered_output 512 (*as output_buffer*)
      
    method input_block_size = 1                            
    method output_block_size = 1                           
      
    method put_substring src ofs len =                     
      if len > 0 then begin
        self#ensure_capacity 256;
        let (_, used_in, used_out) =
          deflate zs
                  src ofs len
                  obuf oend (Bytes.length obuf - oend)
                  Z_NO_FLUSH in
        oend <- oend + used_out;
        if used_in < len
        then self#put_substring src (ofs + used_in) (len - used_in)
      end
      
    method put_string s =
      self#put_substring (Bytes.unsafe_of_string s) 0 (String.length s)
      
    method put_char c = self#put_string (String.make 1 c)
      
    method put_byte b = self#put_char (Char.chr b)
      
    method flush =
      self#ensure_capacity 256;
      let (_, _, used_out) =
         deflate zs
                 Bytes.empty 0 0
                 obuf oend (Bytes.length obuf - oend)
                 Z_SYNC_FLUSH in
      oend <- oend + used_out;
      if oend = Bytes.length obuf then self#flush
      
    method finish =
      (*self#ensure_capacity 256;
      let (finished, _, used_out) =
         deflate zs
                 "" 0 0
                 obuf oend (String.length obuf - oend)
                 Z_FINISH in
      oend <- oend + used_out;
      if finished then*) deflate_end zs (*else self#finish*)

    (*method wipe =
      output_buffer#wipe*)
end

let compress ?(level = 6) () = new compress level

class uncompress =
  object(self)
    val zs = inflate_init true

    inherit buffered_output 512 (*as output_buffer*)

    method input_block_size = 1
    method output_block_size = 1

    method put_substring src ofs len =
      if len > 0 then begin
        self#ensure_capacity 256;
        let (finished, used_in, used_out) =
          inflate zs
                  src ofs len
                  obuf oend (Bytes.length obuf - oend)
                  Z_SYNC_FLUSH in
        oend <- oend + used_out;
        if used_in < len then begin
          if finished then
            raise(Error(Compression_error("Zlib.uncompress",
               "garbage at end of compressed data")));
          self#put_substring src (ofs + used_in) (len - used_in)
        end
      end

    method put_string s =
      self#put_substring (Bytes.unsafe_of_string s) 0 (String.length s)

    method put_char c = self#put_string (String.make 1 c)

    method put_byte b = self#put_char (Char.chr b)

    method flush = ()

    method finish =
      (*let rec do_finish first_finish =
        self#ensure_capacity 256;
        let (finished, _, used_out) =
           inflate zs
                   " " 0 (if first_finish then 1 else 0)
                   obuf oend (String.length obuf - oend)
                   Z_SYNC_FLUSH in
        oend <- oend + used_out;
        if not finished then do_finish false in
      do_finish true;*) inflate_end zs

    (*method wipe =
      output_buffer#wipe*)
end

let uncompress () = new uncompress

end

type zlib_socket =
    {zfd : Eio_unix.Net.stream_socket_ty Eio.Std.r;
     compress : Cryptokit.transform;
     uncompress : Cryptokit.transform;
     mutable buf : Cstruct.t;
     mutable off : int;
     mutable is_closed : bool;
    }

module ZlibSocketMod : Eio.Net.Pi.STREAM_SOCKET
       with type t = zlib_socket and type tag = [ `Generic | `Unix ] =
struct
  type t = zlib_socket
  type tag = [ `Generic | `Unix ]

  let shutdown sock cmd =
    Eio.Flow.shutdown sock.zfd cmd

  let read_methods = []

  let rec single_read sock buf =
    let len = Cstruct.length buf in
    let buflen = Cstruct.length sock.buf - sock.off in
    if buflen > 0 then (
      if buflen <= len then (
	Cstruct.blit sock.buf sock.off buf 0 buflen;
	sock.buf <- Cstruct.empty;
	sock.off <- 0;
	buflen
      ) else (
	Cstruct.blit sock.buf sock.off buf 0 len;
	sock.off <- sock.off + len;
	len
      )
    ) else (
      let n = Eio.Flow.single_read sock.zfd buf in
      if sock.is_closed
      then raise End_of_file;
      sock.uncompress#put_substring (Cstruct.to_bytes buf) 0 n;
      sock.uncompress#flush;
      if sock.uncompress#available_output > 0 then (
	let (ubuf, uoff, ulen) = sock.uncompress#get_substring in
	if ulen <= len then (
	  Cstruct.blit_from_bytes ubuf uoff buf 0 ulen;
	  ulen
	) else (
	  Cstruct.blit_from_bytes ubuf uoff buf 0 len;
	  sock.buf <- Cstruct.of_bytes (Bytes.sub ubuf (uoff + len) (ulen - len));
	  sock.off <- 0;
	  len
	)
      ) else single_read sock buf
    )

  let single_write sock bufs =
    let buf = Cstruct.to_bytes (Cstruct.concat bufs) in
    let len = Bytes.length buf in
    sock.compress#put_substring buf 0 len;
    sock.compress#flush;
    if sock.compress#available_output > 0 then (
      let (buf, off, len) = sock.compress#get_substring in
      let buf = Cstruct.of_bytes ~off ~len buf in
      Eio.Flow.write sock.zfd [buf]
    );
    len

  let copy sock ~src = Eio.Flow.Pi.simple_copy ~single_write sock ~src

  let close sock =
    sock.is_closed <- true;
    (try sock.compress#finish with _ -> ());
    (try sock.uncompress#finish with _ -> ());
    Eio.Flow.close sock.zfd
end

type socket =
  {mutable socket : Eio_unix.Net.stream_socket_ty Eio.Std.r;
   pid : pid;
   mutable writer : unit Eio.Promise.u option;
   read_buffer : Cstruct.t;
   buffer : Buffer.t;
   mutable buffer_limit : int;
   mutable waiters : (unit, exn) result Eio.Promise.u list;
   mutable timeout : float;
   mutable name : mod_name;
  }

type msg +=
   | Tcp_data of socket * string
   | Tcp_close of socket

let rec writer socket =
  let len = Buffer.length socket.buffer in
  if len > 0 then (
    let data = Buffer.to_bytes socket.buffer in
    Buffer.reset socket.buffer;
    Printf.printf "writer %S\n%!" (Bytes.to_string data);
    (try
       Eio.Flow.copy_string (Bytes.unsafe_to_string data) socket.socket;
     with
     | exn ->
        Logs.err ~src
	  (fun m ->
            m "writer raised exception: %a"
              Jamler_log.pp_exn exn);
        Eio.Flow.close socket.socket;
	let senders = socket.waiters in
        socket.waiters <- [];
	List.iter (fun w -> Eio.Promise.resolve_error w exn) senders;
	socket.pid $! Tcp_close socket;
        raise exn
    );
    writer socket
  ) else (
    let senders = socket.waiters in
    socket.waiters <- [];
    List.iter (fun w -> Eio.Promise.resolve_ok w ()) senders;
    if Buffer.length socket.buffer = 0 then (
      let waiter, wakener = Eio.Promise.create () in
      socket.writer <- Some wakener;
      Eio.Promise.await waiter;
      socket.writer <- None;
      writer socket
    ) else writer socket
  )

let of_fd fd pid =
  {socket = fd;
   pid;
   writer = None;
   read_buffer = Cstruct.create 4096;
   buffer = Buffer.create 100;
   buffer_limit = -1;
   waiters = [];
   timeout = -1.0;
   name = `Tcp;
  }

let set_timeout socket t =
  socket.timeout <- t

let set_buffer_limit socket limit =
  socket.buffer_limit <- limit

let close' socket =
  (*Printf.printf "close'\n%!";*)
  Eio.Flow.close socket.socket;
  socket.pid $! Tcp_close socket

let close socket =
  (*Printf.printf "close\n%!";*)
  Eio.Flow.close socket.socket

let buf_size = 4096
let buf = Bytes.make buf_size '\000'

let activate socket pid =
  let cancel = ref None in
  Eio.Fiber.fork
    ~sw:(Process.get_global_switch ())
    (fun () ->
      try
        Eio.Cancel.sub
          (fun c ->
            cancel := Some c;
            match Eio.Flow.single_read socket.socket socket.read_buffer with
            | len ->
               let data = Cstruct.to_string ~len socket.read_buffer in
	       pid $! Tcp_data (socket, data);
            | exception End_of_file ->
               close' socket
          )
      with
      | Eio.Cancel.Cancelled _ -> ()
    );
  match !cancel with
  | None -> assert false
  | Some cancel -> cancel


let send socket data =
  Eio.Flow.copy_string (Bytes.to_string data) socket.socket

let get_name socket =
  socket.name

(*
let starttls socket _opts =
  let module S = (val socket.socket : Socket) in
  let fd = S.SocketMod.get_fd S.socket in
  let context = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  let () = Ssl.use_certificate context "src/ssl.pem" "src/ssl.pem" in
  let ssl_socket = Lwt_ssl.ssl_accept fd context in
  let s =
    (module struct
       type t = Lwt_ssl.socket Lwt.t
       let socket = ssl_socket
       module SocketMod = SSLSocketMod
     end : Socket)
  in
    socket.socket <- s
 *)

(* TODO: wait for eio_ssl update *)
let starttls _socket _opts = ignore (assert false); ()

let zlib_socket_handler = Eio.Net.Pi.stream_socket (module ZlibSocketMod)

let compress socket =
  let fd = socket.socket in
  let compress = Zlib.compress () in
  let uncompress = Zlib.uncompress () in
  let zlib_socket =
    {zfd = fd;
     compress;
     uncompress;
     buf = Cstruct.empty;
     off = 0;
     is_closed = false;
    }
  in
  let s = Eio.Resource.T (zlib_socket, zlib_socket_handler) in
  socket.socket <- s;
  socket.name <- `Zlib
