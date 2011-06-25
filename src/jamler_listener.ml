open Process

module C2SServer = Jamler_c2s.C2SServer

let rec accept listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
    ignore (C2SServer.start socket);
    accept listen_socket

let listener_start port self =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept socket

let start port =
  ignore (spawn (listener_start port))
