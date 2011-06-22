open Process

type socket = {fd : Lwt_unix.file_descr;
	       pid : msg pid;
	       mutable writer : unit Lwt.u option;
	       mutable buffer : Buffer.t;
	       mutable buffer_limit : int;
	       mutable waiters : unit Lwt.u list;
	       mutable timeout : float;
	      }

and msg =
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
  ignore (Lwt_unix.close socket.fd);
  Buffer.reset socket.buffer;
  socket.pid $! `Tcp_close socket

let close socket =
  lwt () = Lwt_unix.close socket.fd in
    Buffer.reset socket.buffer;
    Lwt.return ()

let buf_size = 4096
let buf = String.make buf_size '\000'

let activate socket pid =
  ignore (
    try_lwt
      lwt len = Lwt_unix.read socket.fd buf 0 buf_size in
        if len > 0 then (
	  let data = String.sub buf 0 len in
	    pid $! `Tcp_data (socket, data)
	) else (
	  close' socket
	);
        Lwt.return ()
    with
      | exn ->
	  lwt () = Lwt_unix.close socket.fd in
	  lwt () =
            Lwt_io.eprintf "Reader raised exception: %s\n"
	      (Printexc.to_string exn)
          in
	  let senders = socket.waiters in
            socket.waiters <- [];
	    List.iter (fun w -> Lwt.wakeup_exn w exn) senders;
	    socket.pid $! `Tcp_close socket;
            Lwt.fail exn
  )

let state socket = Lwt_unix.state socket.fd

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

