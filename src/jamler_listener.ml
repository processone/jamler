open Process

let section = Jamler_log.new_section "s2s"

type t = C2S | S2S | Service

module C2SServer = Jamler_c2s.C2SServer
module Service = Jamler_service.Service
module S2SInServer = Jamler_s2s_in.S2SInServer

let rec accept listen_type listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
  let sockaddr_to_string addr =
    let nameinfo =
      Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
    in
      nameinfo.Unix.ni_hostname ^ ":" ^ nameinfo.Unix.ni_service
  in
  lwt () =
    Lwt_log.notice_f
      ~section
      "Accepted connection %s -> %s"
      (sockaddr_to_string (Lwt_unix.getpeername socket))
      (sockaddr_to_string (Lwt_unix.getsockname socket))
  in
  (match listen_type with
    | C2S -> ignore (C2SServer.start socket)
    | Service -> ignore (Service.start socket)
    | S2S -> ignore (S2SInServer.start socket));
  accept listen_type listen_socket

let start_listener listen_type port _self =
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
    | 5269 ->
      ignore (spawn (start_listener S2S port))
    | 5270 ->
      ignore (spawn (start_listener Service port))

let start_listeners () =
  List.iter start_listener [5222; 5269; 5270]
