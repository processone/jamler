open Process

module XMLReceiver = Jamler_receiver
module GenServer = Gen_server
module LJID = Jlib.LJID
module LJIDSet = Jlib.LJIDSet
module Hooks = Jamler_hooks
module Auth = Jamler_auth
module SASL = Jamler_sasl
module Router = Jamler_router
module GenIQHandler = Jamler_gen_iq_handler
module SM = Jamler_sm
module Local = Jamler_local
module C2S = Jamler_c2s.C2S
module C2SServer = Jamler_c2s.C2SServer




let myhosts () =
  List.map Jlib.nameprep_exn ["localhost"; "e.localhost"] (* TODO *)


let rec accept listen_socket =
  lwt (socket, _) = Lwt_unix.accept listen_socket in
    ignore (C2SServer.start socket);
    accept listen_socket

let listener_start () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, 5222) in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.bind socket addr;
    Lwt_unix.listen socket 1024;
    accept socket

let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(*
let _ =
  List.iter Sql.add_pool ((myhosts ()) :> string list);
  let user = "test10" in
  let query =
    <:sql< SELECT @(password)s from users where username = %(user)s >>
  in
  lwt [p] = Sql.query "e.localhost" query in
    Lwt_io.printf "pwd %s\n" p
*)

let (exit_waiter, exit_wakener) = Lwt.wait ()

let main () =
  let _ = listener_start () in
    exit_waiter

let () = Lwt_main.run (main ())
