open Process

let section = Jamler_log.new_section "listener"

module type ListenModule =
sig
  val name : string
  val listen_parser : (Lwt_unix.file_descr -> empty pid) Jamler_config.p
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
  let%lwt (socket, _) = Lwt_unix.accept listen_socket in
  let peername = Lwt_unix.getpeername socket in
  let sockname = Lwt_unix.getsockname socket in
  let%lwt () =
    Lwt_log.notice_f
      ~section
      "accepted connection %s -> %s"
      (sockaddr_to_string peername)
      (sockaddr_to_string sockname)
  in
  let pid = start socket in
  let%lwt () =
    Lwt_log.notice_f
      ~section
      "%a is handling connection %s -> %s"
      format_pid pid
      (sockaddr_to_string peername)
      (sockaddr_to_string sockname)
  in
    accept start listen_socket

let start_listener (port, family, start) _self =
  let socket, addr =
    match family with
      | INET ->
	  Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0,
	  Unix.ADDR_INET (Unix.inet_addr_any, port)
      | INET6 ->
	  Lwt_unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0,
	  Unix.ADDR_INET (Unix.inet6_addr_any, port)
  in
    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
    Lwt_unix.setsockopt socket Unix.TCP_NODELAY true;
    let%lwt () = Lwt_unix.bind socket addr in
    Lwt_unix.listen socket 1024;
    accept start socket

let start_listeners () =
  List.iter
    (fun data ->
       ignore (spawn (start_listener data))
    ) (listeners ())
