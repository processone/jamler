open Process

type t = C2S | S2S | Service

module C2SServer = Jamler_c2s.C2SServer
module Service = Jamler_service.Service

let rec accept listen_type listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
  (match listen_type with
    | C2S -> ignore (C2SServer.start socket);
    | Service -> ignore (Service.start socket);
    | S2S -> ());
  accept listen_type listen_socket

let start_listener listen_type port self =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept listen_type socket

let start_listener port =
  (* TODO: fix this crappy shit *)
  match port with
    | 5222 ->
      ignore (spawn (start_listener C2S port))
    | 5270 ->
      ignore (spawn (start_listener Service port))

let start_listeners () =
  List.iter start_listener [5222; 5270]
