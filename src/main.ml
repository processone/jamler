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
  List.iter Sql.add_pool (Jamler_config.myhosts ());
  let user = "test10" in
  let query =
    <:sql< SELECT @(password)s from users where username = %(user)s >>
  in
  let query2 =
    <:sql< UPDATE users SET password='test' where username=%(user)s >>
  in
    try_lwt
      lwt [p] =
	Sql.transaction (Jlib.nameprep_exn "e.localhost")
	  (fun () ->
	     lwt p = Sql.query_t query in
	     lwt () = Lwt_unix.sleep 5.0 in
	     lwt _ = Sql.query_t query2 in
	     lwt p' = Sql.query_t query in
	       Lwt.return p
	  )
      in
	Lwt_io.printf "pwd %s\n" p
    with
      | Sql.Error (desc, fields) as exn ->
	  let sfields = List.map (fun (c, s) -> Printf.sprintf "(%c, %s) " c s) fields in
	  Lwt_log.error_f ~exn:exn "sql pg query %s" (String.concat "" sfields)
      | exn ->
	  Lwt_log.error ~exn:exn "sql query"
*)

module Plugins = Plugins

let section = Jamler_log.new_section "main"

(* Start all the modules in all the hosts *)
let start_modules () =
  Lwt_list.iter_s
    (fun host ->
       let modules = Jamler_config.modules host in
	 Lwt_list.iter_s
	   (fun (mod_name, opts) ->
	      Gen_mod.start_module host mod_name opts
	   ) modules
    ) (Jamler_config.myhosts ())


let (exit_waiter, exit_wakener) = Lwt.wait ()

let config_file_path = ref ""
let pid_file_path = ref ""

let main () =
  lwt () = Jamler_config.read_config !config_file_path in
  Jamler_local.start ();
  List.iter Sql.add_pool (Jamler_config.myhosts ());
  lwt () = start_modules () in
  let _ = Listener.start_listeners () in
  lwt () = Lwt_log.notice_f ~section
    "jamler %s started using ocaml-%s @ %s/%s/%s"
    Cfg.version Cfg.ocaml Cfg.arch Cfg.system Cfg.os in
    exit_waiter

let usage = Printf.sprintf "Usage: %s -c file [-p file]" Sys.argv.(0)

let () = 
  let speclist = [("-c", Arg.String (fun s -> config_file_path := s),
		   "filename  Path to configuation file");
		  ("-p", Arg.String (fun s -> pid_file_path := s),
		   "filename  Path to PID file")] in
    Arg.parse speclist (fun _ -> ()) usage;
    match !config_file_path with
      | "" ->
	  Arg.usage speclist usage
      | _ ->
	  (try
	     Lwt_main.run (main ())
	   with
	     | Yojson.Json_error err ->
		 Lwt.ignore_result
		   (Lwt_log.fatal_f ~section
		      "%s: %s" !config_file_path err)
	     | Unix.Unix_error (err, _,  _) ->
		 Lwt.ignore_result
		   (Lwt_log.fatal_f ~section
		      "%s: %s" !config_file_path (Unix.error_message err))
	     | Jamler_config.Error err ->
		 Lwt.ignore_result
		   (Lwt_log.fatal_f ~section
		      "%s: %s" !config_file_path err))
