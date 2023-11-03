open Process

let section = Jamler_log.new_section "socket"

let rec really_write write socket str pos len =
  let%lwt n = write socket str pos len in
    if len = n
    then Lwt.return ()
    else really_write write socket str (pos + n) (len - n)

type mod_name = [ `Tcp | `SSL | `Zlib ]

module type SocketMod =
sig
  type t
  val read : t -> bytes -> int -> int -> int Lwt.t
  val write : t -> bytes -> int -> int -> int Lwt.t
  val close : t -> unit Lwt.t
  val get_fd : t -> Lwt_unix.file_descr
  val name : mod_name
end

module type Socket =
sig
  type t
  val socket : t
  module SocketMod : SocketMod with type t = t
end

module TcpSocketMod : SocketMod with type t = Lwt_unix.file_descr =
struct
  type t = Lwt_unix.file_descr
  let read = Lwt_unix.read
  let write = Lwt_unix.write
  let close = Lwt_unix.close
  let get_fd socket = socket
  let name = `Tcp
end

let _ =
  Ssl.init ()

type tls_option = [ `Certfile of string | `Connect ]

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
    {zfd : Lwt_unix.file_descr;
     compress : Cryptokit.transform;
     uncompress : Cryptokit.transform;
     mutable buf : bytes;
     mutable off : int;
    }

module ZlibSocketMod : SocketMod with type t = zlib_socket =
struct
  type t = zlib_socket
  let rec read sock buf off len =
    let buflen = Bytes.length sock.buf - sock.off in
      if buflen > 0 then (
	if buflen <= len then (
	  Bytes.blit sock.buf sock.off buf off buflen;
	  sock.buf <- Bytes.empty;
	  sock.off <- 0;
	  Lwt.return buflen
	) else (
	  Bytes.blit sock.buf sock.off buf off len;
	  sock.off <- sock.off + len;
	  Lwt.return len
	)
      ) else (
	let%lwt n = Lwt_unix.read sock.zfd buf off len in
	  if n > 0 then (
	    sock.uncompress#put_substring buf off n;
	    sock.uncompress#flush;
	    if sock.uncompress#available_output > 0 then (
	      let (ubuf, uoff, ulen) = sock.uncompress#get_substring in
		if ulen <= len then (
		  Bytes.blit ubuf uoff buf off ulen;
		  Lwt.return ulen
		) else (
		  Bytes.blit ubuf uoff buf off len;
		  sock.buf <- Bytes.sub ubuf (uoff + len) (ulen - len);
		  sock.off <- 0;
		  Lwt.return len
		)
	    ) else read sock buf off len
	  ) else Lwt.return 0
      )
  let write sock buf off len =
    sock.compress#put_substring buf off len;
    sock.compress#flush;
    let%lwt () =
      if sock.compress#available_output > 0 then (
	let (buf, off, len) = sock.compress#get_substring in
	let%lwt () = really_write Lwt_unix.write sock.zfd buf off len in
	  Lwt.return ()
      ) else Lwt.return ()
    in
      Lwt.return len
  let close sock =
    (try sock.compress#finish with _ -> ());
    (try sock.uncompress#finish with _ -> ());
    Lwt_unix.close sock.zfd
  let get_fd _socket = assert false
  let name = `Zlib
end

type socket = {mutable socket : (module Socket);
	       pid : msg pid;
	       mutable writer : unit Lwt.u option;
	       buffer : Buffer.t;
	       mutable buffer_limit : int;
	       mutable waiters : unit Lwt.u list;
	       mutable timeout : float;
	      }

and msg =
    [ `Tcp_data of socket * string
    | `Tcp_close of socket ]

let rec writer socket =
  let len = Buffer.length socket.buffer in
    if len > 0 then (
      let data = Buffer.to_bytes socket.buffer in
	Buffer.reset socket.buffer;
	let%lwt () =
	  try%lwt
	    let module S = (val socket.socket : Socket) in
	      really_write S.SocketMod.write S.socket data 0 len
          with
	    | exn ->
		let module S = (val socket.socket : Socket) in
	        let%lwt () = S.SocketMod.close S.socket in
		let%lwt () =
                  Lwt_log.error ~exn ~section "writer raised exception"
                in
		let senders = socket.waiters in
                  socket.waiters <- [];
	          List.iter (fun w -> Lwt.wakeup_exn w exn) senders;
		  socket.pid $! `Tcp_close socket;
                  Lwt.fail exn
        in
	  writer socket
    ) else (
      let senders = socket.waiters in
	socket.waiters <- [];
	List.iter (fun w -> Lwt.wakeup w ()) senders;
	if Buffer.length socket.buffer = 0 then (
	  let waiter, wakener = Lwt.wait () in
	    socket.writer <- Some wakener;
	    let%lwt () = waiter in
	      socket.writer <- None;
	      writer socket
        ) else writer socket
    )

let of_fd fd pid =
  let s =
    (module struct
       type t = Lwt_unix.file_descr
       let socket = fd
       module SocketMod = TcpSocketMod
     end : Socket)
  in
  let socket =
    {socket = s;
     pid = (pid :> msg pid);
     writer = None;
     buffer = Buffer.create 100;
     buffer_limit = -1;
     waiters = [];
     timeout = -1.0;
    }
  in
    ignore (writer socket);
    socket

let set_timeout socket t =
  socket.timeout <- t

let set_buffer_limit socket limit =
  socket.buffer_limit <- limit

let close' socket =
  let module S = (val socket.socket : Socket) in
    ignore (S.SocketMod.close S.socket);
    Buffer.reset socket.buffer;
    socket.pid $! `Tcp_close socket

let close socket =
  let module S = (val socket.socket : Socket) in
  let%lwt () =
    Lwt.catch
      (fun () -> S.SocketMod.close S.socket)
      (fun _ -> Lwt.return ())
  in
    Buffer.reset socket.buffer;
    Lwt.return ()

let buf_size = 4096
let buf = Bytes.make buf_size '\000'

let activate socket pid =
  try%lwt
    let module S = (val socket.socket : Socket) in
    let%lwt len = S.SocketMod.read S.socket buf 0 buf_size in
      if len > 0 then (
	let data = Bytes.sub_string buf 0 len in
	  pid $! `Tcp_data (socket, data)
      ) else (
	close' socket
      );
      Lwt.return ()
  with
    | Lwt.Canceled ->
	Lwt.return ()
    | exn ->
	let%lwt () =
	  match exn with
	    | Unix.Unix_error(Unix.EBADF, _, _) ->
		Lwt.return ()
	    | _ ->
		Lwt_log.error ~exn ~section "reader raised exception"
        in
	let module S = (val socket.socket : Socket) in
	let%lwt () =
	  try%lwt
	    S.SocketMod.close S.socket
	  with
	    | Unix.Unix_error(Unix.EBADF, _, _) ->
		Lwt.return ()
	    | exn ->
		Lwt_log.error ~exn ~section "error closing socket"
	in
	let senders = socket.waiters in
          socket.waiters <- [];
	  List.iter (fun w -> Lwt.wakeup_exn w exn) senders;
	  socket.pid $! `Tcp_close socket;
          Lwt.fail exn

(*
let state socket =
  let module S = (val socket.socket : Socket) in
    S.SocketMod.state S.socket

exception Closed
*)

let send' socket data =
  (*let module S = (val socket.socket : Socket) in
    if S.SocketMod.state S.socket <> Lwt_unix.Opened
    then raise Closed
    else*) (
      match socket.writer with
	| None ->
	    Buffer.add_bytes socket.buffer data
	| Some writer ->
	    Buffer.add_bytes socket.buffer data;
	    Lwt.wakeup writer ()
    )

let send socket data =
  let waiter, wakener = Lwt.wait () in
    socket.waiters <- wakener :: socket.waiters;
    if socket.timeout <= 0.0 then (
      send' socket data;
      waiter
    ) else (
      Lwt_unix.with_timeout socket.timeout
	(fun () ->
	   send' socket data;
	   waiter
	)
    )

let send_async socket data =
  if socket.buffer_limit >= 0 &&
    socket.buffer_limit < Buffer.length socket.buffer
  then (
    close' socket
  );
  ignore (
    try%lwt
      send socket data
    with
      | Lwt_unix.Timeout as exn ->
	  close' socket;
	  Lwt.fail exn
  )

let get_name socket =
  let module S = (val socket.socket : Socket) in
    S.SocketMod.name

let starttls socket _opts =
  let module S = (val socket.socket : Socket) in
  let fd = S.SocketMod.get_fd S.socket in
  let context = Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context in
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

let compress socket =
  let module S = (val socket.socket : Socket) in
  let fd = S.SocketMod.get_fd S.socket in
  let compress = Zlib.compress () in
  let uncompress = Zlib.uncompress () in
  let zlib_socket =
    {zfd = fd;
     compress;
     uncompress;
     buf = Bytes.empty;
     off = 0;
    }
  in
  let s =
    (module struct
       type t = zlib_socket
       let socket = zlib_socket
       module SocketMod = ZlibSocketMod
     end : Socket)
  in
    socket.socket <- s
