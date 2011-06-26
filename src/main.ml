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
module Listener = Jamler_listener




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

module Plugins = Plugins

let (exit_waiter, exit_wakener) = Lwt.wait ()

let main () =
  let _ = Listener.start_listeners () in
    exit_waiter

let () = Lwt_main.run (main ())
