module SASL = Jamler_sasl

module SASLDigest =
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

  type step = One | Three | Five

  type state =
      {step : step;
       nonce : string;
       username : string;
       authzid : string;
       get_password : SASL.get_password;
       check_password_digest : SASL.check_password_digest;
       auth_module : string;
       host : Jlib.namepreped;
      }

  let get_assoc_s = Xml.get_attr_s

  let parse s =
    let rec parse1 s i k ts =
      if i < String.length s then (
	match s.[i] with
	  | '=' ->
	      parse2 s (i + 1) (String.sub s k (i - k)) ts
	  | ' '
	  | ',' when i = k ->
	      parse1 s (i + 1) (i + 1) ts
	  | _c ->
	      parse1 s (i + 1) k ts
      ) else (
	if i = k
	then Some (List.rev ts)
	else None
      )
    and parse2 s i key ts =
      if i < String.length s then (
	match s.[i] with
	  | '"' ->
	      parse3 s (i + 1) key (Buffer.create 10) ts
	  | c ->
	      let v = Buffer.create 10 in
		Buffer.add_char v c;
		parse4 s (i + 1) key v ts
      ) else None
    and parse3 s i key v ts =
      if i < String.length s then (
	match s.[i] with
	  | '"' ->
	      parse4 s (i + 1) key v ts
	  | '\\' when i < String.length s - 1 ->
	      Buffer.add_char v s.[i + 1];
	      parse3 s (i + 2) key v ts
	  | c ->
	      Buffer.add_char v c;
	      parse3 s (i + 1) key v ts
      ) else None
    and parse4 s i key v ts =
      if i < String.length s then (
	match s.[i] with
	  | ',' ->
	      parse1 s (i + 1) (i + 1) ((key, Buffer.contents v) :: ts)
	  | ' ' ->
	      parse4 s (i + 1) key v ts
	  | c ->
	      Buffer.add_char v c;
	      parse4 s (i + 1) key v ts
      ) else parse1 s i i ((key, Buffer.contents v) :: ts)
    in
      parse1 s 0 0 []


  (*
    @doc Check if the digest-uri is valid.
    RFC-2831 allows to provide the IP address in Host,
    however ejabberd doesn't allow that.
    If the service (for example jabber.example.org)
    is provided by several hosts (being one of them server3.example.org),
    then digest-uri can be like xmpp/server3.example.org/jabber.example.org
    In that case, ejabberd only checks the service name, not the host.
  *)
  let is_digesturi_valid digest_uri_case jabber_host =
    let digest_uri = Stringprep.tolower digest_uri_case in
      match tokenize '/' digest_uri with
	| ["xmpp"; host] when host = jabber_host ->
	    true
	| ["xmpp"; _host; servname] when servname = jabber_host ->
	    true
	| _ ->
	    false


  let hex s =
    let t = Cryptokit.Hexa.encode () in
      Cryptokit.transform_string t s

  let response key_vals user passwd nonce authzid a2prefix =
    let realm = get_assoc_s "realm" key_vals in
    let cnonce = get_assoc_s "cnonce" key_vals in
    let digest_uri = get_assoc_s "digest-uri" key_vals in
    let nc = get_assoc_s "nc" key_vals in
    let qop = get_assoc_s "qop" key_vals in
    let a1 =
      match authzid with
	| "" ->
	    Jlib.md5 (user ^ ":" ^ realm ^ ":" ^ passwd) ^
	      ":" ^ nonce ^ ":" ^ cnonce
	| _ ->
	    Jlib.md5 (user ^ ":" ^ realm ^ ":" ^ passwd) ^
	      ":" ^ nonce ^ ":" ^ cnonce ^ ":" ^ authzid
    in
    let a2 =
      match qop with
	| "auth" ->
	    a2prefix ^ ":" ^ digest_uri;
	| _ ->
	    a2prefix ^ ":" ^ digest_uri ^
	      ":00000000000000000000000000000000"
    in
    let t =
      hex (Jlib.md5 a1) ^ ":" ^ nonce ^ ":" ^
	nc ^ ":" ^ cnonce ^ ":" ^ qop ^ ":" ^
	hex (Jlib.md5 a2)
    in
      hex (Jlib.md5 t)


  let rec mech_step state client_in =
    match state, client_in with
    | {step = One; nonce = nonce; _}, _ ->
       SASL.Continue
	 ("nonce=\"" ^ nonce ^
	    "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
	  mech_step {state with step = Three})
    | {step = Three; nonce = nonce; _}, client_in -> (
      match parse client_in with
      | None ->
	 SASL.Error "bad-protocol"
      | Some key_vals -> (
	let digest_uri = get_assoc_s "digest-uri" key_vals in
	let username = get_assoc_s "username" key_vals in
	match
	  is_digesturi_valid digest_uri (state.host :> string),
	  Jlib.nodeprep username
	with
	| true, Some lusername -> (
	  let authzid = get_assoc_s "authzid" key_vals in
	  match state.get_password lusername with
	  | None ->
	     SASL.ErrorUser ("not-authorized", username)
	  | Some (passwd, auth_module) -> (
	    match state.check_password_digest
		    lusername ""
		    (get_assoc_s "response" key_vals)
		    (fun pw ->
		      response key_vals username pw
			nonce authzid "AUTHENTICATE")
	    with
	    | Some _ ->
	       let rsp_auth =
		 response key_vals
		   username passwd
		   nonce authzid ""
	       in
	       SASL.Continue
		 ("rspauth=" ^ rsp_auth,
		  mech_step
		    {state with
		      step = Five;
		      auth_module;
		      username;
		      authzid}
		 )
	    | None ->
	       SASL.ErrorUser
		 ("not-authorized", username)
	  )
	)
	| _, _ ->
	   SASL.ErrorUser ("not-authorized", username)
      )
    )
    | {step = Five; auth_module; username; authzid; _}, "" ->
       SASL.Done [(`Username, username);
		  (`Authzid, authzid);
		  (`Auth_module, auth_module)];
    | {step = Five; _}, _ ->
       SASL.Error "bad-protocol"


  let mech_new host get_password _check_password check_password_digest
      client_in =
    let state =
      {step = One;
       nonce = Jlib.get_random_string ();
       username = "";
       authzid = "";
       auth_module = "";
       host;
       get_password;
       check_password_digest}
    in
      mech_step state client_in



end

let _ =
  SASL.register_mechanism "DIGEST-MD5" (module SASLDigest : SASL.SASLMechanism)

