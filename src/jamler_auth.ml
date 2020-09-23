module Hooks = Jamler_hooks

let section = Jamler_log.new_section "auth"

type result = | OK
	      | Empty_password
	      | Not_allowed
	      | Invalid_jid
	      | Exists
	      | Server_error

let remove_user = Hooks.create ()
let register_user = Hooks.create ()

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
  val plain_password_required : bool

  val does_user_exist : Jlib.nodepreped -> Jlib.namepreped -> bool Lwt.t
  val remove : Jlib.nodepreped -> Jlib.namepreped -> unit Lwt.t
  val remove' : Jlib.nodepreped -> Jlib.namepreped -> string -> unit Lwt.t
  val try_register : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t
  val set_password : Jlib.nodepreped -> Jlib.namepreped -> string -> result Lwt.t

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
		   "auth module %s not found" m
	       );
	       raise exn
      ) methods

let check_password_with_authmodule user server password =
  let rec aux user server password =
    function
      | [] -> Lwt.return None
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match%lwt A.check_password user server password with
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
	    match%lwt (A.check_password_digest user server password
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
	    match%lwt A.get_password user server with
	      | Some password ->
		  Lwt.return (Some (password, A.name))
	      | None ->
		  aux user server mods
	)
  in
    aux user server (auth_modules server)

let plain_password_required server =
  List.exists
    (fun m ->
       let module A = (val m : Auth) in
	 A.plain_password_required
    ) (auth_modules server)


let does_user_exist user server =
  let rec aux user server =
    function
      | [] -> Lwt.return false
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match%lwt A.does_user_exist user server with
	      | true ->
		  Lwt.return true
	      | false ->
		  aux user server mods
	)
  in
    aux user server (auth_modules server)

let set_password user server password =
  if password = "" then Lwt.return Empty_password
  else
    let rec aux user server password = function
      | [] -> Lwt.return Not_allowed
      | m :: mods -> (
	  let module A = (val m : Auth) in
	    match%lwt A.set_password user server password with
	      | OK ->
		  Lwt.return OK
	      | _ ->
		  aux user server password mods
	)
    in
      aux user server password (auth_modules server)
    
let try_register user server password =
  if password = "" then Lwt.return Empty_password
  else
    match%lwt does_user_exist user server with
      | true ->
	  Lwt.return Exists
      | false ->
	  if Jamler_config.is_my_host server then (
	    let rec aux user server password = function
	      | [] -> Lwt.return Not_allowed
	      | m :: mods -> (
		  let module A = (val m : Auth) in
		    match%lwt A.try_register user server password with
		      | OK ->
			  Lwt.return OK
		      | _ ->
			  aux user server password mods
		)
	    in
	      match%lwt aux user server password (auth_modules server) with
		| OK ->
		    let%lwt _ = Hooks.run register_user server (user, server) in
		      Lwt.return OK
		| res ->
		    Lwt.return res
	  ) else
	    Lwt.return Not_allowed

let remove user server =
  let%lwt _ = Lwt_list.iter_s
    (fun m ->
       let module A = (val m : Auth) in
	 A.remove user server)
    (auth_modules server) in
  let%lwt _ = Hooks.run remove_user server (user, server) in
    Lwt.return ()

let remove' user server password =
  let%lwt _ = Lwt_list.iter_s
    (fun m ->
       let module A = (val m : Auth) in
	 A.remove' user server password)
    (auth_modules server) in
  let%lwt _ = Hooks.run remove_user server (user, server) in
    Lwt.return ()

let entropy s =
  match String.length s with
    | 0 ->
	0.0
    | len ->
	let digit = ref 0 in
	let printable = ref 0 in
	let lowletter = ref 0 in
	let hiletter = ref 0 in
	let other = ref 0 in
	  String.iter
	    (fun c ->
	       if c >= 'a' && c <= 'z' then
		 lowletter := 26
               else if c >= '0' && c <= '9' then
		 digit := 9
	       else if c >= 'A' && c <= 'Z' then
		 hiletter := 26
	       else if (Char.code c) >= 0x21 && (Char.code c) <= 0x7e then
		 printable := 33
	       else
		 other := 128) s;
	  let sum = !digit + !printable + !lowletter + !hiletter + !other in
	    (float_of_int len) *. (log (float_of_int sum)) /. (log 2.0)

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

  let plain_password_required = false

  let does_user_exist _user _server =
    Lwt.return true

  let remove _user _server =
    Lwt.return ()

  let remove' _user _server _password =
    Lwt.return ()

  let set_password _user _server _password =
    Lwt.return OK

  let try_register _user _server _password =
    Lwt.return OK

end

let () =
  register_mod (module BenignAuth : Auth)
