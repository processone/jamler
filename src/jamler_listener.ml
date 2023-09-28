open Process

let src = Jamler_log.new_src "listener"

module type ListenModule =
sig
  val name : string
  val listen_parser :
    (Eio_unix.Net.stream_socket_ty Eio.Std.r ->
     pid) Jamler_config.p
end

let mods : (string, (module ListenModule)) Hashtbl.t =
  Hashtbl.create 10

let register_mod mod' =
  let module M = (val mod' : ListenModule) in
    Hashtbl.replace mods M.name mod'

type family = INET | INET6

module JSON = Yojson.Safe

let listener_p name =
  Jamler_config.(
    P (fun json ->
	 try
	   match json with
	     | (`Assoc assoc) as json ->
		 let port =
		   try
		     match List.assoc "port" assoc with
		       | `Int port -> port
		       | json ->
			   raise (Error
				    (Printf.sprintf
				       "port number must be an integer, got %s"
				       (JSON.to_string json)))
		   with
		     | Not_found ->
			 raise (Error "port number must be present")
		 in
		 let family =
		   try
		     let json = List.assoc "family" assoc in
		     let (P p) = enum [("ipv4", INET); ("ipv6", INET6)] in
		       p json
		   with
		     | Not_found -> INET
		 in
		 let mod_name =
		   try
		     match List.assoc "module" assoc with
		       | `String mod_name -> mod_name
		       | json ->
			   raise (Error
				    (Printf.sprintf
				       "module name must be a string, got %s"
				       (JSON.to_string json)))
		   with
		     | Not_found ->
			 raise (Error "module name must be present")
		 in
		 let m =
		   try
		     Hashtbl.find mods mod_name
		   with
		     | Not_found ->
			 raise (Error
				  (Printf.sprintf
				     "Unknown listener module name: %s"
				     mod_name))
		 in
		 let module M = (val m : ListenModule) in
		 let P p = M.listen_parser in
		   (port, family, p json)
	     | json ->
		 raise (Error
			  (Printf.sprintf "expected JSON object value, got %s"
			     (JSON.to_string json)))
	 with
	   | Error s ->
	       raise (Error (Printf.sprintf "error in %s listener: %s"
			       name s))
      )
  )


let listeners =
  Jamler_config.(get_global_opt_with_default ["listen"] (assoc listener_p) [])

let sockaddr_to_string addr =
  let nameinfo =
    Unix.getnameinfo addr [Unix.NI_NUMERICHOST; Unix.NI_NUMERICSERV]
  in
    nameinfo.Unix.ni_hostname ^ ":" ^ nameinfo.Unix.ni_service

let rec accept start listen_socket =
  let (socket, _peer) =
    Eio.Net.accept
      ~sw:(Process.get_global_switch ())
      listen_socket
  in
  let fd = Eio_unix.Net.fd socket in
  let peername = Eio_unix.Fd.use_exn "getpeername" fd Unix.getpeername in
  let sockname = Eio_unix.Fd.use_exn "getsockname" fd Unix.getsockname in
  Logs.info ~src
    (fun m ->
      m "accepted connection %s -> %s"
        (sockaddr_to_string peername)
        (sockaddr_to_string sockname));
  let pid = start socket in
  Logs.info ~src
    (fun m ->
      m "%a is handling connection %s -> %s"
        pp_pid pid
        (sockaddr_to_string peername)
        (sockaddr_to_string sockname));
  accept start listen_socket

let start_listener (port, family, start) _self =
  let addr =
    match family with
    | INET ->
       `Tcp (Eio.Net.Ipaddr.V4.any, port)
    | INET6 ->
       `Tcp (Eio.Net.Ipaddr.V6.any, port)
  in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~backlog:1024
      ~sw:(Process.get_global_switch ())
      (Process.get_global_env ())#net
      addr
  in
  accept start socket

let start_listeners () =
  List.iter
    (fun data ->
      ignore (
          spawn (start_listener data))
    ) (listeners ())
