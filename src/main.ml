(*open Process*)

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
module Listener = Jamler_listener


let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Dependencies *)
module C2SServer = Jamler_c2s.C2SServer
module Service = Jamler_service.Service
module S2SInServer = Jamler_s2s_in.S2SInServer
module Plugins = Plugins

let src = Jamler_log.new_src "main"

(* Start all the modules in all the hosts *)
let start_modules () =
  List.iter
    (fun host ->
      let modules = Jamler_config.modules host in
      List.iter
	(fun mod_name ->
	  Gen_mod.start_module host mod_name
	) modules
    ) (Jamler_config.myhosts ())


let (exit_waiter, exit_wakener) = Eio.Promise.create ()

let config_file_path = ref ""
let pid_file_path = ref ""
let name = ref "jamler@localhost"
let cookie = ref ""

let make_abs_path filename =
  match Filename.is_relative filename with
  | true ->
     let cwd = Sys.getcwd () in
     Filename.concat cwd filename
  | false ->
     filename

let process_pid_file () =
  match !pid_file_path with
  | "" -> ()
  | _ ->
     pid_file_path := make_abs_path !pid_file_path;
     Logs.info ~src
       (fun m ->
         m "using pid file \"%s\"" !pid_file_path);
     try
       Eio.Path.save
         ~create:(`Or_truncate 0o640)
         Eio.Path.((Process.get_global_env ())#fs / !pid_file_path)
         (string_of_int (Unix.getpid ()))
     with
     | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied (Eio_unix.Unix_error (err, _,  _))), _context) ->
        Logs.err ~src
	  (fun m ->
            m "%S: %s" !pid_file_path (Unix.error_message err))

let main () =
  process_pid_file ();
  Jamler_router.start ();
  Jamler_sm.start ();
  Erl_epmd.start_net ();
  Erl_epmd.start !name !cookie;
  ignore (Jamler_cluster.start ());
  Jamler_config.read_config !config_file_path;
  Jamler_captcha.check_captcha_setup ();
  Jamler_local.start ();
  List.iter Sql.add_pool (Jamler_config.myhosts ());
  start_modules ();
  Listener.start_listeners ();
  Logs.info ~src
    (fun m -> m "jamler %s started using ocaml-%s @ %s/%s/%s"
                Cfg.version Cfg.ocaml Cfg.arch Cfg.system Cfg.os
    );
  Eio.Promise.await exit_waiter

let usage =
  Printf.sprintf "Usage: %s -c file [-p file] [-name name] [-cookie cookie]"
    Sys.argv.(0)

let remove_pid_file () =
  try Sys.remove !pid_file_path
  with | _ -> ()

let () = 
  at_exit remove_pid_file;
  let speclist =
    [("-c", Arg.Set_string config_file_path,
      "filename  Path to configuation file");
     ("-p", Arg.Set_string pid_file_path,
      "filename  Path to PID file");
     ("-name", Arg.Set_string name,
      "name  Name of the node in the form of node@domain");
     ("-cookie", Arg.Set_string cookie,
      "cookie  Erlang cookie");
    ]
  in
  Arg.parse speclist (fun _ -> ()) usage;
  match !config_file_path with
  | "" ->
     Arg.usage speclist usage
  | _ ->
     config_file_path := make_abs_path !config_file_path;
     (*Lwt_engine.set (new Lwt_engine.libev ());*)
     Eio_main.run
       (fun env ->
         Process.set_global_env env;
         Eio.Switch.run
           (fun sw ->
             Process.set_global_switch sw;
             (*Lwt_eio.with_event_loop ~clock:env#clock ~debug:true
               (fun () ->*)
	         try
                    main ()
	          with
	          | Yojson.Json_error err ->
                     Logs.err ~src
	               (fun m ->
                         m "%S: %s" !config_file_path err)
	          | Unix.Unix_error (err, _,  _) ->
                     Logs.err ~src
	               (fun m ->
                         m "%S: %s" !config_file_path (Unix.error_message err));
	          | Jamler_config.Error err ->
                     Logs.err ~src
	               (fun m ->
                         m "%S: %s" !config_file_path err)
           (* ) *)
           )
       )
