let rec loop c =
  lwt () = Lwt_io.printf "%c" c in
  lwt () = Lwt_io.flush Lwt_io.stdout in
    loop c

let rec loop2 c n =
  for_lwt i = 1 to n do
    lwt () = Lwt_io.printf "%c %d\n" c i in
      Lwt.return ()
  done

(*
let main () =
  lwt () = loop2 'A' 1000
  and () = loop2 'B' 100 in
    Lwt.return ()
*)

type -'a pid

type 'a proc = {queue : 'a Queue.t;
		mutable wakener : 'a Lwt.u option}

external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"

let spawn f =
  let proc = {queue = Queue.create ();
	      wakener = None}
  in
  let pid = proc_to_pid proc in
  let _ =
    try_lwt
      f pid
    with
      | exn ->
	  lwt () =
            Lwt_io.eprintf "Process raised exception: %s\n"
	      (Printexc.to_string exn)
	  in
            Lwt.fail exn
  in
    pid

exception Queue_limit

let send pid msg =
  let proc = pid_to_proc pid in
    (match proc.wakener with
       | None ->
	   if Queue.length proc.queue > 10000
	   then raise Queue_limit;
	   Queue.add msg proc.queue
       | Some wakener ->
	   Lwt.wakeup wakener msg
    )

let ($!) = send

let receive pid =
  let proc = pid_to_proc pid in
    if Queue.is_empty proc.queue then (
      let (waiter, wakener) = Lwt.wait () in
	proc.wakener <- Some wakener;
	lwt msg = waiter in
          proc.wakener <- None;
          Lwt.return msg
    ) else (
      Lwt.return (Queue.take proc.queue)
    )

let (exit_waiter, exit_wakener) = Lwt.wait ()

let rec loop3 c self =
  lwt msg = receive self in
    match msg with
      | `Bla (from, n) ->
	  if n <= 100 then (
	    lwt () = Lwt_io.printf "%c %d\n" c n in
              from $! `Bla (self, n + 1);
              loop3 c self
          ) else (
	    lwt () = Lwt_io.printf "exit %c %d\n" c n in
	      Lwt.wakeup exit_wakener ();
	      Lwt.return ()
            (*loop3 c self*)
	  )

let rec loop4 proc from self =
  proc $! `Bla (from, -100);
  proc $! `Bla (from, -100);
  lwt () = Lwt.pause () in
    loop4 proc from self

(*
let main () =
  let p1 = spawn (loop3 'A')
  and p2 = spawn (loop3 'B') in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
  let _ = spawn (loop4 p1 p2) in
    p1 $! `Bla (p2, 0);
    (*Lwt.return ()*)
    exit_waiter

let () = Lwt_main.run (main ())
*)

module Tcp =
struct
  type socket = {fd : Lwt_unix.file_descr;
		 pid : tcp_msg pid;
		 mutable writer : unit Lwt.u option;
		 mutable buffer : Buffer.t;
		 mutable buffer_limit : int;
		 mutable waiters : unit Lwt.u list;
		 mutable timeout : float;
		}

  and tcp_msg =
      [ `Tcp_data of socket * string
      | `Tcp_close of socket ]

  let rec writer socket =
    let rec write socket str pos len =
      lwt n = Lwt_unix.write socket.fd str pos len in
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
	          lwt () = Lwt_unix.close socket.fd in
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
    let socket =
      {fd;
       pid = (pid :> tcp_msg pid);
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
    ignore (Lwt_unix.close socket.fd);
    Buffer.reset socket.buffer;
    socket.pid $! `Tcp_close socket

  let buf_size = 4096
  let buf = String.make buf_size '\000'

  let activate socket pid =
    ignore (
      lwt len = Lwt_unix.read socket.fd buf 0 buf_size in
        if len > 0 then (
	  let data = String.sub buf 0 len in
	    pid $! `Tcp_data (socket, data)
	) else (
	  close' socket
	);
        Lwt.return ()
    )

  exception Closed

  let send' socket data =
    if Lwt_unix.state socket.fd <> Lwt_unix.Opened
    then raise Closed
    else (
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


  (*let send' socket data =
    let rec write socket str pos len =
      lwt n = Lwt_unix.write socket.fd str pos len in
        if len = n
	then Lwt.return ()
	else write socket str (pos + n) (len - n)
    in
    let len = String.length data in
      socket.writer <-
	socket.writer >> write socket data 0 len;
      socket.writer

  let send socket data =
    if socket.timeout <= 0.0
    then send' socket data
    else Lwt_unix.with_timeout socket.timeout (fun () -> send' socket data)

  let send_async socket data =
    if socket.buffer_limit >= 0 && socket.buffer_limit < socket.buffered
    then (socket.pid $! `Tcp_close socket;
    ignore (
      try_lwt
	send socket data
      with
	| Lwt_unix.Timeout as exn ->
	    socket.pid $! `Tcp_close socket;
	    Lwt.fail exn
    )
  *)

end


module C2S :
sig
  type msg = [ Tcp.tcp_msg | `Zxc of string * int ]
  val start : Lwt_unix.file_descr -> msg pid -> unit Lwt.t
end =
struct
  type msg = [ Tcp.tcp_msg | `Zxc of string * int ]

  type state = {pid : msg pid;
		socket : Tcp.socket}

  let rec loop state =
    lwt msg = receive state.pid in
      match msg with
	| `Tcp_data (socket, data) when socket == state.socket ->
	    lwt () = Lwt_io.printf "tcp data %d %S\n" (String.length data) data in
	      Tcp.activate state.socket state.pid;
	      state.pid $! `Zxc (data, 1);
	      loop state
	| `Tcp_data (_socket, _data) -> assert false
	| `Tcp_close socket when socket == state.socket ->
	    lwt () = Lwt_io.printf "tcp close\n" in
              Gc.print_stat stdout;
              Gc.compact ();
              Gc.print_stat stdout; flush stdout;
	      Lwt.return ()
	| `Tcp_close _socket ->
	    assert false
	| `Zxc (s, n) ->
	    if n <= 1000000 then (
	      Tcp.send_async state.socket (string_of_int n ^ s);
	      state.pid $! `Zxc (s, n + 1)
	    );
	    Lwt_main.yield () >>
	    loop state

  let start socket self =
    let socket = Tcp.of_fd socket self in
    let state = {pid = self;
		 socket = socket} in
      Tcp.activate socket self;
      loop state
end



let rec accept listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
    ignore (spawn (C2S.start socket));
    accept listen_socket

let listener_start () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, 5222) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept socket

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let main () =
  let _ = listener_start () in
    exit_waiter

let () = Lwt_main.run (main ())
