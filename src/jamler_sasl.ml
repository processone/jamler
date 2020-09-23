type get_password = Jlib.nodepreped -> (string * string) option Lwt.t
type check_password = Jlib.nodepreped -> string -> string option Lwt.t
type check_password_digest =
    Jlib.nodepreped -> string -> string -> (string -> string) ->
      string option Lwt.t

type props = ([ `Username | `Auth_module | `Authzid ] * string) list

type step_result =
  | Done of props
  | Continue of string * t
  | ErrorUser of string * string
  | Error of string
and t = string -> step_result Lwt.t

module type SASLMechanism =
sig
  val mech_new : Jlib.namepreped -> get_password ->
    check_password -> check_password_digest -> string -> step_result Lwt.t
end

let mechanisms : (string, (module SASLMechanism)) Hashtbl.t =
  Hashtbl.create 10

let register_mechanism mech_name mech_mod =
  Hashtbl.replace mechanisms mech_name mech_mod

let listmech _server =
  Hashtbl.fold (fun name _ acc -> name :: acc) mechanisms []

let check_credentials props =
  try
    let user = List.assoc `Username props in
      match (Jlib.nodeprep user :> string option) with
	| Some ""
	| None -> Error "not-authorized"
	| _ -> Done props
  with
    | Not_found ->
	Error "not-authorized"

let rec process_mech_result =
  function
    | Done props ->
	check_credentials props
    | Continue (server_out, f) ->
	Continue (server_out,
		  fun s ->
		    let%lwt res = f s in
		      Lwt.return (process_mech_result res)
		 )
    | (ErrorUser _ | Error _) as error -> error

let server_start ~service:_service ~server_fqdn ~user_realm:_user_realm
    ~get_password ~check_password ~check_password_digest ~mech client_in =
  try
    let mech_mod = Hashtbl.find mechanisms mech in
    let module Mech = (val mech_mod : SASLMechanism) in
    let%lwt mech =
      Mech.mech_new server_fqdn
	get_password check_password check_password_digest client_in
    in
      Lwt.return (process_mech_result mech)
  with
    | Not_found ->
	Lwt.return (Error "no-mechanism")

let server_step f client_in =
  let%lwt res = f client_in in
    Lwt.return (process_mech_result res)

