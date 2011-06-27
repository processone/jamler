let section = Jamler_log.new_section "auth"

module type Auth =
sig
  val name : string

  val check_password :
    Jlib.nodepreped -> Jlib.namepreped -> string -> bool Lwt.t
  val check_password_digest :
    Jlib.nodepreped -> Jlib.namepreped ->
    string -> string -> (string -> string) -> bool Lwt.t
  val get_password :
    Jlib.nodepreped -> Jlib.namepreped -> string option Lwt.t

  val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool Lwt.t
end

let mods : (string, (module Auth)) Hashtbl.t =
  Hashtbl.create 10

let register_mod auth_mod =
  let module A = (val auth_mod : Auth) in
    Hashtbl.replace mods A.name auth_mod

let auth_modules server =
  let methods = Jamler_config.auth_modules server in
    List.map
      (fun m ->
	 try
	   Hashtbl.find mods m
	 with
	   | Not_found as exn ->
	       ignore (
		 Lwt_log.error_f
		   ~section
		   ~exn:exn
		   "Auth module %s not found" m
	       );
	       raise exn
      ) methods

let check_password_with_authmodule user server password =
  let rec aux user server password =
    function
      | [] -> Lwt.return None
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match_lwt A.check_password user server password with
	      | true ->
		  Lwt.return (Some A.name)
	      | false ->
		  aux user server password mods
	)
  in
    aux user server password (auth_modules server)

let check_password_digest_with_authmodule user server password
    digest digest_gen =
  let rec aux user server password digest digest_gen =
    function
      | [] -> Lwt.return None
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match_lwt (A.check_password_digest user server password
			 digest digest_gen) with
	      | true ->
		  Lwt.return (Some A.name)
	      | false ->
		  aux user server password digest digest_gen mods
	)
  in
    aux user server password digest digest_gen (auth_modules server)


let get_password_with_authmodule user server =
  let rec aux user server =
    function
      | [] -> Lwt.return None
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match_lwt A.get_password user server with
	      | Some password ->
		  Lwt.return (Some (password, A.name))
	      | None ->
		  aux user server mods
	)
  in
    aux user server (auth_modules server)

let does_user_exist user server =
  let rec aux user server =
    function
      | [] -> Lwt.return false
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match_lwt A.does_user_exist user server with
	      | true ->
		  Lwt.return true
	      | false ->
		  aux user server mods
	)
  in
    aux user server (auth_modules server)


module BenignAuth : Auth =
struct
  let name = "benign"

  let check_password _user _server _password =
    Lwt.return true

  let check_password_digest
      _user _server _password
      _digest _digest_gen =
    Lwt.return true

  let get_password _user _server =
    Lwt.return (Some "test")

  let does_user_exist _user _server =
    Lwt.return true
end

let () =
  register_mod (module BenignAuth : Auth)
