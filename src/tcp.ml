open Process

type mod_name = [ `Tcp | `SSL | `Zlib ]

module type SocketMod =
sig
  type t
  val read : t -> string -> int -> int -> int Lwt.t
  val write : t -> string -> int -> int -> int Lwt.t
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
    lwt sock = sock in
      Lwt_ssl.read sock buf off len
  let write sock buf off len =
    lwt sock = sock in
      Lwt_ssl.write sock buf off len
  let close sock =
    lwt sock = sock in
      Lwt_ssl.close sock
  let get_fd _socket = assert false
  let name = `SSL
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
  let rec write socket str pos len =
    let module S = (val socket.socket : Socket) in
    lwt n = S.SocketMod.write S.socket str pos len in
      if len = n
      then Lwt.return ()
      else write socket str (pos + n) (len - n)
  in
  let len = Buffer.length socket.buffer in
    if len > 0 then (
      let data = Buffer.contents socket.buffer in
	Buffer.reset socket.buffer;
	lwt () =
	  try_lwt
	    write socket data 0 len
          with
	    | exn ->
		let module S = (val socket.socket : Socket) in
	        lwt () = S.SocketMod.close S.socket in
		lwt () =
                  Lwt_io.eprintf "Writer raised exception: %s\n"
		    (Printexc.to_string exn)
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
	    lwt () = waiter in
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
  lwt () =
    Lwt.catch
      (fun () -> S.SocketMod.close S.socket)
      (fun _ -> Lwt.return ())
  in
    Buffer.reset socket.buffer;
    Lwt.return ()

let buf_size = 4096
let buf = String.make buf_size '\000'

let activate socket pid =
  try_lwt
    let module S = (val socket.socket : Socket) in
    lwt len = S.SocketMod.read S.socket buf 0 buf_size in
      if len > 0 then (
	let data = String.sub buf 0 len in
	  pid $! `Tcp_data (socket, data)
      ) else (
	close' socket
      );
      Lwt.return ()
  with
    | Lwt.Canceled ->
	Lwt.return ()
    | exn ->
	let module S = (val socket.socket : Socket) in
	lwt () = S.SocketMod.close S.socket in
	lwt () =
          Lwt_io.eprintf "Reader raised exception: %s\n"
	    (Printexc.to_string exn)
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
	    Buffer.add_string socket.buffer data
	| Some writer ->
	    Buffer.add_string socket.buffer data;
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
    try_lwt
      send socket data
    with
      | Lwt_unix.Timeout as exn ->
	  close' socket;
	  Lwt.fail exn
  )

let get_name socket =
  let module S = (val socket.socket : Socket) in
    S.SocketMod.name

let starttls socket opts =
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
