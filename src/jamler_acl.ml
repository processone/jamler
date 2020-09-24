(* ACL stub *)
(*
type rule = string				(* TODO *)
type acl = string				(* TODO *)
*)

open Jamler_config
module JSON = Yojson.Safe

let make_acl key v =
  match key with
    | "user" -> (
	try
	  ignore (String.index v '@');
	  match Jlib.string_to_jid v with
	    | Some jid ->
		let luser = jid.Jlib.luser
		and lserver = jid.Jlib.lserver in
		  (fun _host jid ->
		     jid.Jlib.luser = luser &&
		      jid.Jlib.lserver = lserver
		  )
	    | None ->
		raise (Error (Printf.sprintf "Incorrect JID: %S" v))
	with
	  | Not_found -> (
	      match Jlib.nodeprep v with
		| Some luser ->
		    (fun host jid ->
		       jid.Jlib.luser = luser &&
			(match host with
			   | Some host -> jid.Jlib.lserver = host
			   | None -> Jamler_config.is_my_host jid.Jlib.lserver
			)
		    )
		| None ->
		    raise (Error (Printf.sprintf "Incorrect username: %S" v))
	    )
      )
    | _ ->
	raise (Error (Printf.sprintf "Wrong ACL type: %s" key))

let parse_acl =
  P (function
       | `Assoc [(key, `String v)] -> (
	   make_acl key v
	 )
       | `Assoc [(_key, json)] ->
	   raise (Error (Printf.sprintf
			   "expected string, got %s"
			   (JSON.to_string json)))
       | (`Assoc _assoc) as json ->
	   raise (Error (Printf.sprintf
			   "ACL JSON object must have only one field, got %s"
			   (JSON.to_string json)))
       | json ->
	   raise (Error (Printf.sprintf "expected JSON object, got %s"
			   (JSON.to_string json)))
    )

let get_acl =
  function
    | "all" -> fun _ -> [fun _ _ -> true]
    | "none" -> fun _ -> [fun _ _ -> false]
    | name ->
	Jamler_config.get_opt_with_default
	  ["acl"; name] (list parse_acl) []

let get_global_acl =
  function
    | "all" -> fun _ -> [fun _ _ -> true]
    | "none" -> fun _ -> [fun _ _ -> false]
    | name ->
	Jamler_config.get_global_opt_with_default
	  ["acl"; name] (list parse_acl) []

let parse_access (P p) =
  P (function
       | `Assoc [(acl_name, v)] ->
	   let global_acl = get_global_acl acl_name in
	   let acl = get_acl acl_name in
	     (global_acl, acl, p v)
       | (`Assoc _assoc) as json ->
	   raise (Error (Printf.sprintf
			   "Access JSON object must have only one field, got %s"
			   (JSON.to_string json)))
       | json ->
	   raise (Error (Printf.sprintf "expected JSON object, got %s"
			   (JSON.to_string json)))
    )

let access =
  P (function
       | `Bool x -> x
       | `String "deny" -> false
       | `String "allow" -> true
       | json ->
	   raise (Error (Printf.sprintf
			   "expected bool, \"allow\", or \"deny\", got %s"
			   (JSON.to_string json)))
    )

type 'a access_rule =
    (unit ->
       ((unit -> (Jlib.namepreped option -> Jlib.jid -> bool) list) *
          (Jlib.namepreped ->
	     (Jlib.namepreped option -> Jlib.jid -> bool) list) * 'a)
         list) *
      (Jlib.namepreped ->
         ((unit -> (Jlib.namepreped option -> Jlib.jid -> bool) list) *
            (Jlib.namepreped ->
	       (Jlib.namepreped option -> Jlib.jid -> bool) list) * 'a)
           list)

let get_rule name p =
  (Jamler_config.get_global_opt_with_default
     ["access"; name] (list (parse_access p)) [],
   Jamler_config.get_opt_with_default
     ["access"; name] (list (parse_access p)) []
  )

let match_acl host global_acl acl jid =
  let acls =
    match host with
      | Some host -> global_acl () @ acl host
      | None -> global_acl ()
  in
    List.exists (fun f -> f host jid) acls

let rec match_acls host acls jid default =
  match acls with
    | [] -> default
    | (global_acl, acl, v) :: acls ->
	if match_acl host global_acl acl jid
	then v
	else match_acls host acls jid default


let match_rule host (global_rule, rule') jid default =
  let rules = global_rule () @ rule' host in
    match_acls (Some host) rules jid default
let match_global_rule (global_rule, _rule') jid default =
  let rules = global_rule () in
    match_acls None rules jid default

let all =
  ((fun () -> [((fun () -> [fun _ _ -> true]), (fun _ -> []), true)]),
   (fun _ -> [])
  )

let none =
  ((fun () -> [((fun () -> [fun _ _ -> true]), (fun _ -> []), false)]),
   (fun _ -> [])
  )

let none_string =
  ((fun () -> [((fun () -> [fun _ _ -> true]), (fun _ -> []), "none")]),
   (fun _ -> [])
  )

