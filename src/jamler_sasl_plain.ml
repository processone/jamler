module SASL = Jamler_sasl

module SASLPlain =
struct
  let tokenize ?(remove_empty = false) c str =
    let rec aux str from res =
      if from >= String.length str
      then List.rev res
      else 
	try
	  let idx = String.index_from str from c in
	  let token = String.sub str from (idx - from) in
	    match token with
	      | "" when remove_empty -> aux str (idx + 1) res
	      | _ -> aux str (idx + 1) (token :: res)
	with
	  | Not_found ->
	      let str = String.sub str from (String.length str - from) in
		aux "" 0 (str :: res)
    in
      aux str 0 []

  let parse s = tokenize '\000' s
  let parse_domain s = tokenize '@' s

  let prepare client_in =
    match parse client_in with
      | [""; user_maybe_domain; password] -> (
	  match parse_domain user_maybe_domain with
	    | [user; _domain] ->
		(* <NUL>login@domain<NUL>pwd *)
		Some (user_maybe_domain, user, password);
	    | [user] ->
		(* <NUL>login<NUL>pwd *)
		Some ("", user, password)
	    | _ ->
		None
	)
      | [authzid; user; password] ->
	  (* login@domain<NUL>login<NUL>pwd *)
	  Some (authzid, user, password)
      | _ ->
	  None


  let mech_new _host _get_password check_password _check_password_digest
        client_in =
    match prepare client_in with
    | Some (authzid, user, password) -> (
      match Jlib.nodeprep user with
      | Some user' -> (
	match check_password user' password with
	| Some auth_module ->
	   SASL.Done [(`Username, user);
		      (`Authzid, authzid);
		      (`Auth_module, auth_module)]
	| _ ->
	   SASL.ErrorUser ("not-authorized", user)
      )
      | None ->
	 SASL.ErrorUser ("not-authorized", user)
    )
    | None ->
       SASL.Error "bad-protocol"

end

let _ =
  SASL.register_mechanism "PLAIN" (module SASLPlain : SASL.SASLMechanism)

